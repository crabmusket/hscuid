# hscuid

[![Build Status](https://travis-ci.org/eightyeight/hscuid.svg?branch=master)](https://travis-ci.org/eightyeight/hscuid)
[![Hackage](https://img.shields.io/hackage/v/hscuid.svg)](https://hackage.haskell.org/package/hscuid)

A Haskell port of the [JavaScript library][cuid] for collision-resistant identifiers.
To install, [`cabal install hscuid`][hscuid].

## What is a CUID?

CUIDs are short random strings designed so that you can generate a lot of them over many different machines and not get collisions.
They are intended to be usable in situations from HTML element IDs to database keys.
You can read more about them at [usecuid.org][].

## How do I use this library?

```haskell
>>> import Web.Cuid (newCuid, newSlug)
>>> newCuid
"cib3c3tcu0000zwowx9ho2gh4"
>>> newSlug
"y900001wmf"
```

[cuid]: https://github.com/ericelliott/cuid
[hscuid]: https://hackage.haskell.org/package/hscuid
[semver]: http://semver.org
[usecuid.org]: https://usecuid.org
