# hscuid [![Build Status](https://travis-ci.org/eightyeight/hscuid.svg?branch=master)](https://travis-ci.org/eightyeight/hscuid)

A Haskell port of the [JavaScript library][cuid] for collision-resistant identifiers.
To install, [`cabal install hscuid`][hscuid].

## What is a CUID?

CUIDs are short random strings designed so that you can generate a lot of them over many different machines and not get collisions.
They are designed to be usable in many situations, such as HTML element IDs.
You can read more about them at [usecuid.org][].

## How do I use this library?

See documentation on [hackage][hscuid].

[cuid]: https://github.com/ericelliott/cuid
[hscuid]: http://hackage.haskell.org/package/hscuid
[usecuid.org]: https://usecuid.org
