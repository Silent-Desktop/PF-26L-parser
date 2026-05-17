{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO ()
import qualified Data.Text.IO as TIO
import Data.Void
import Debug.Trace (trace, traceM)
import Parser
import Text.Megaparsec

data ParseState = ParseState
  { currentIndent :: Int,
    indentStack :: [Int],
    currentBlock :: Block,
    canHaveElse :: Bool,
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

processLines :: [Line] -> ParseState -> IO ()
processLines [] state = do
  putStr $ showBlocks (currentBlock state)
  return ()
processLines (line@(Line expr comment indent sourcePos) : rest) state = do
  -- processLine line
  -- putStrLn $ "Require indent: " ++ show (requireIndent state)
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
          traceM $
            "Got out of block at line: "
              ++ show
                sourcePos
          -- let (block, stack) = pop (blockStack state)
          -- -- traceM $ "Popped stack: " ++ show block
          -- case block of
          --   Nothing -> do
          --     -- traceM $ "Current block: " ++ show (currentBlock state)
          --     fail $ "Something went wrong"
          --   Just blockSafe -> do
          --     -- print (currentBlock state)
          --     let newBlock = blockSafe {content = content blockSafe ++ [BBlock (currentBlock state)]}
          --     let newState = processLinesRest line state {currentBlock = newBlock, blockStack = stack}
          --     processLines rest newState
          processLines
            rest
            (recurseBlocks indent (currentIndent state) line state)
        else do
          -- putStrLn ""
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
processLinesRest line@(Line expr comment indent sourcePos) state = do
  case expr of
    _ | isBlockStart expr -> do
      -- Entered a block
      let newBlock = Block {header = Just line, content = [], isRoot = False}
      let newStack = push (currentBlock state) (blockStack state)
      let newState = state {requireIndent = True, currentBlock = newBlock, blockStack = newStack, currentIndent = indent, canHaveElse = True}
      -- trace ("Started a block: " ++ show (blockStack newState)) newState
      newState
    ElseExpr -> do
      let newBlock = Block {header = Just line, content = [], isRoot = False}
      let newStack = push (currentBlock state) (blockStack state)
      let newState = state {requireIndent = True, currentBlock = newBlock, blockStack = newStack, currentIndent = indent, canHaveElse = False}
      newState
    Comment _ -> do
      let newState = state {requireIndent = False, canHaveElse = False}
      newState
    _ -> do
      let newBlock = Block {header = header (currentBlock state), content = content (currentBlock state) ++ [BLine line], isRoot = isRoot (currentBlock state)}
      let newState = state {requireIndent = False, currentBlock = newBlock, currentIndent = indent, canHaveElse = False}
      newState

isBlockStart :: Expr -> Bool
isBlockStart (IfExpr _) = True
isBlockStart (ElifExpr _) = True
isBlockStart (WhileLoop _) = True
isBlockStart (ForLoop _ _) = True
isBlockStart (FuncDeclExpr _ _) = True
isBlockStart (Class _ _) = True
isBlockStart _ = False

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

processLine :: Line -> IO ()
processLine (Line expr comment indent sourcePos) = do
  putStrLn $
    "Line "
      ++ show sourcePos
      ++ " (indent "
      ++ show indent
      ++ "): "
      ++ show expr
      ++ maybe "" (\c -> " | " ++ show c) comment

main :: IO ()
main = do
  result <- parseProgram "example2.py"
  let state = ParseState {currentIndent = 0, blockStack = [], currentBlock = Block {header = Nothing, content = [], isRoot = True}, canHaveElse = False, indentSize = 4, requireIndent = False, indentStack = []}
  case result of
    Left err -> putStrLn $ errorBundlePretty err
    Right ls -> processLines ls state