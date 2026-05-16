# This is a comment

# Variable assignments
x = 1
y = 2.5
z = "hello"
w = "world"
b = True
n = False

# Multiline string
m = """
this is a
multiline string
"""

# Walrus operator
a = (v := 10)

# Math expressions
add = x + y
sub = x - y
mul = x * y
div = x / y
chain = x + y - z

# Boolean math expressions
gt = x > y
lt = x < y
gte = x >= y
lte = x <= y
eq = x == y
neq = x != y

# Boolean logic expressions
beq = b == True
bneq = b != False
veq = x == y

# Boolean operators
and_expr = x > 0 and y > 0
or_expr = x > 0 or y > 0
chain_bool = x > 0 and y > 0 or z == "hello"

# Ternary
ternary = x if x > 0 else y
nested_ternary = x if x > 0 else y if y > 0 else 0

# Function calls
f(x)
f(x, y)
f(x, y, z=1)
f(x, y=1, z=2)
f()

# Nested calls
f(g(x))
f(g(x, y=1), z=2)

# List comprehensions
simple_comp = [x for x in y]
comp_with_if = [x for x in y if x > 0]
comp_with_ifs = [x for x in y if x > 0 if x < 10]
comp_with_ternary = [x if x > 0 else 0 for x in y]
comp_with_range = [x for x in range(10)]
comp_with_call = [f(x) for x in y]
nested_comp = [x for x in [y for y in z]]
comp_add = [x for x in y] + [z for z in w]
comp_mult = [x for x in y] * 2

# Ternary with list comp
ternary_comp = x if x > 0 else [y for y in z]
comp_as_value = [x for x in y] if True else z

# Complex nested expressions
result = [
    f(x, y=z) if (x := g(True)) > 0 else [y for y in range(10) if y != False]
    for x in h(1, 2)
    if x == True
    if x <= 10
]

# If statements with bodies
if x > 0:
    y = 1

if x == True:
    y = 1

if x > 0 and y > 0:
    z = 1

if x > 0 or y > 0:
    z = 1

if x != y:
    z = 1

# If elif else
if x > 0:
    z = 1
elif x == 0:
    z = 2
else:
    z = 3

# While loops
while x > 0:
    x = x - 1

while x > 0 and y > 0:
    x = x - 1

# For loops
for i in range(10):
    x = x + i

for i in range(1, 10):
    x = x + i

for i in f(y):
    x = x + i


# Function declarations
def simple():
    return x


def with_args(a, b):
    return a + b


def with_defaults(a, b, c=1):
    return a + b + c


def complex_func(a, b, c=1, d=2):
    result = a + b
    return result if result > 0 else c + d


# Class declarations
class Foo:
    x = 1


class Bar(Foo):
    y = 2


# Multiline string as function call argument
f("""
this is a
multiline string
""")

f(
    x,
    """
another
multiline
string
""",
    y=1,
)
