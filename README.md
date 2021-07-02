# flipstone-prelude

This package and its primary module `Flipstone.Prelude` is intended to be used
in place of the default Haskell `Prelude` module.

It represents a set of preferences for a default prelude that we commonly use
on projects at Flipstone.

Generally speaking this prelude is like many others in that we want to remove
the unsafe parts of the standard prelude (like `head` and to a lesser extent
`foldl`) while including conveniences from commonly used packages like `extra`.
