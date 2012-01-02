String interpolation for Haskell:

    >>> :set -fth
    >>> let x = "bar"
    >>> let y = 23
    >>> $(format "foo {x} {y} baz")
    "foo bar 23 baz"
