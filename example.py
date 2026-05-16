if __name__ == "__main__":
    x = 1 + 1
    print(x)
    return [f(x, y, z=1) if x > 2 else g(y, z, x=1) for p in [x for x in z] if x == 1]


def funcName(x, y, z, p=None):
    """Multiline
    comment"""
    # This is a comment
    y = 1 + 2  # This is another comment
    if x == y and y or z and p == True or z == p:
        print("Hello")
