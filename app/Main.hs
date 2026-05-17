{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO ()
import qualified Data.Text.IO as TIO
import Data.Void
import Parser
import Text.Megaparsec

data ParseState = ParseState
  { currentIndent :: Int,
    indentStack :: [Int],
    currentBlock :: Block,
    indentSize :: Int,
    requireIndent :: Bool,
    blockStack :: Stack
  }
  deriving (Show)

data Block = Block
  { header :: Maybe Line,
    isRoot :: Bool,
    content :: [BlockContent]
  }
  deriving (Show)

data BlockContent = BBlock Block | BLine Line deriving (Show)

type Stack = [Block]

showBlocks :: Block -> String
showBlocks = showBlocksIndent 0

showBlocksIndent :: Int -> Block -> String
showBlocksIndent indent block =
  let prefix = replicate indent ' '
      headerStr = case header block of
        Nothing -> prefix ++ "<root>\n"
        Just line -> prefix ++ show line ++ "\n"
      contentStr = concatMap (showContent (indent + 4)) (content block)
   in headerStr ++ contentStr

showContent :: Int -> BlockContent -> String
showContent indent (BLine line) = replicate indent ' ' ++ show line ++ "\n"
showContent indent (BBlock block) = showBlocksIndent indent block

getExpr :: Line -> Expr
getExpr (Line expr _ _ _) = expr

isElse :: Line -> Bool
isElse line = case getExpr line of
  ElseExpr -> True
  _ -> False

isElif :: Line -> Bool
isElif line = case getExpr line of
  ElifExpr _ -> True
  _ -> False

checkElse :: Maybe BlockContent -> BlockContent -> Either String ()
checkElse prevBlockC bc = case bc of
  BLine _ -> Right ()
  BBlock bblock ->
    case header bblock of
      Nothing ->
        let prevs = Nothing : map Just (init (content bblock))
            pairs = zip prevs (content bblock)
         in mapM_ (uncurry checkElse) pairs
      Just blockHeader ->
        if isElse blockHeader
          then case prevBlockC of
            Nothing -> Left $ "Invalid else at: " ++ show (getLineSourcePos blockHeader)
            Just prevBlockCd -> case prevBlockCd of
              BLine _ -> Left $ "Invalid else at: " ++ show (getLineSourcePos blockHeader)
              BBlock prevBlock ->
                case header prevBlock of
                  Just prev
                    | canHaveElse (getExpr prev) ->
                        let prevs = Nothing : map Just (init (content bblock))
                            pairs = zip prevs (content bblock)
                         in mapM_ (uncurry checkElse) pairs
                  _ -> Left $ "Invalid else at: " ++ show (getLineSourcePos blockHeader)
          else
            if isElif blockHeader
              then case prevBlockC of
                Nothing -> Left $ "Invalid elif at: " ++ show (getLineSourcePos blockHeader)
                Just prevBlockCd -> case prevBlockCd of
                  BLine _ -> Left $ "Invalid elif at: " ++ show (getLineSourcePos blockHeader)
                  BBlock prevBlock ->
                    case header prevBlock of
                      Just prev
                        | canHaveElif (getExpr prev) ->
                            let prevs = Nothing : map Just (init (content bblock))
                                pairs = zip prevs (content bblock)
                             in mapM_ (uncurry checkElse) pairs
                      _ -> Left $ "Invalid elif at: " ++ show (getLineSourcePos blockHeader)
              else
                let prevs = Nothing : map Just (init (content bblock))
                    pairs = zip prevs (content bblock)
                 in mapM_ (uncurry checkElse) pairs

push :: Block -> Stack -> Stack
push x xs = x : xs

pop :: Stack -> (Maybe Block, Stack)
pop [] = (Nothing, [])
pop (x : xs) = (Just x, xs)

peek :: Stack -> Maybe Block
peek [] = Nothing
peek (x : _) = Just x

parseProgram :: FilePath -> IO (Either (ParseErrorBundle Text Void) [Line])
parseProgram path = do
  contents <- TIO.readFile path
  return $ parse program path contents

isComment :: Expr -> Bool
isComment (Comment _) = True
isComment _ = False

getLineSourcePos :: Line -> SourcePos
getLineSourcePos _line@(Line _ _ _ sourcePos) = sourcePos

processLines :: [Line] -> ParseState -> IO ()
processLines [] state = do
  case checkElse Nothing (BBlock (currentBlock state)) of
    Left err -> fail $ "Semantic error: " ++ err
    Right () -> putStrLn "OK"
  putStr $ showBlocks (currentBlock state)
  return ()
processLines (line@(Line expr _ indent sourcePos) : rest) state = do
  if requireIndent state && indent /= currentIndent state + indentSize state
    then
      fail $
        "invalid indentation at line "
          ++ show sourcePos
          ++ ", expected "
          ++ show (currentIndent state + indentSize state)
          ++ " got "
          ++ show indent
    else
      if indent < currentIndent state && not (isComment expr)
        then do
          processLines
            rest
            (recurseBlocks indent (currentIndent state) line state)
        else do
          let newState = processLinesRest line state
          processLines rest newState

recurseBlocks :: Int -> Int -> Line -> ParseState -> ParseState
recurseBlocks indent destIndent line state
  | indent >= destIndent = processLinesRest line state
  | otherwise = do
      let (block, stack) = pop (blockStack state)
       in case block of
            Nothing -> error "recurseBlocks: empty stack, something went wrong"
            Just blockSafe ->
              let newBlock =
                    blockSafe
                      { content = content blockSafe ++ [BBlock (currentBlock state)]
                      }
                  newState =
                    state
                      { currentBlock = newBlock,
                        blockStack = stack
                      }
               in recurseBlocks indent (destIndent - indentSize state) line newState

processLinesRest :: Line -> ParseState -> ParseState
processLinesRest line@(Line expr _ indent _) state = do
  case expr of
    _ | isBlockStart expr -> do
      let newBlock = Block {header = Just line, content = [], isRoot = False}
      let newStack = push (currentBlock state) (blockStack state)
      let newState = state {requireIndent = True, currentBlock = newBlock, blockStack = newStack, currentIndent = indent}
      newState
    ElseExpr -> do
      let newBlock = Block {header = Just line, content = [], isRoot = False}
      let newStack = push (currentBlock state) (blockStack state)
      let newState = state {requireIndent = True, currentBlock = newBlock, blockStack = newStack, currentIndent = indent}
      newState
    Comment _ -> do
      let newState = state {requireIndent = False}
      newState
    _ -> do
      let newBlock = Block {header = header (currentBlock state), content = content (currentBlock state) ++ [BLine line], isRoot = isRoot (currentBlock state)}
      let newState = state {requireIndent = False, currentBlock = newBlock, currentIndent = indent}
      newState

isBlockStart :: Expr -> Bool
isBlockStart (IfExpr _) = True
isBlockStart (ElifExpr _) = True
isBlockStart (WhileLoop _) = True
isBlockStart (ForLoop _ _) = True
isBlockStart (FuncDeclExpr _ _) = True
isBlockStart (Class _ _) = True
isBlockStart _ = False

canHaveElse :: Expr -> Bool
canHaveElse (IfExpr _) = True
canHaveElse (ElifExpr _) = True
canHaveElse (WhileLoop _) = True
canHaveElse _ = False

canHaveElif :: Expr -> Bool
canHaveElif (IfExpr _) = True
canHaveElif (ElifExpr _) = True
canHaveElif _ = False

canEndIndent :: Expr -> Bool
canEndIndent (Assign _ _) = True
canEndIndent (ForLoop _ _) = True
canEndIndent expr = isValuable expr

isValuable :: Expr -> Bool
isValuable (Number _) = True
isValuable (FloatNum _) = True
isValuable (Lit _) = True
isValuable Literal = True
isValuable MathExpr = True
isValuable (Add _ _) = True
isValuable (Walrus _ _) = True
isValuable (Sub _ _) = True
isValuable (Div _ _) = True
isValuable (Mult _ _) = True
isValuable (BoolLogicExpr {}) = True
isValuable (BoolMathExpr {}) = True
isValuable (BinOp {}) = True
isValuable (ListCompExpr {}) = True
isValuable _ = False

main :: IO ()
main = do
  result <- parseProgram "example2.py"
  let state = ParseState {currentIndent = 0, blockStack = [], currentBlock = Block {header = Nothing, content = [], isRoot = True}, indentSize = 4, requireIndent = False, indentStack = []}
  case result of
    Left err -> putStrLn $ errorBundlePretty err
    Right ls -> processLines ls state