String interpolation for Haskell.

Here is an interactive session that shows how it is used:

    >>> :set -fth
    >>> let x = "bar"
    >>> let y = 23
    >>> $(format "foo {x} {y} baz")
    "foo bar 23 baz"
