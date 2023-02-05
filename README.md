# `elm-oklch` [![Build Status](https://github.com/miniBill/elm-oklch/workflows/CI/badge.svg)](https://github.com/miniBill/elm-oklch/actions?query=branch%3Amain)
This package implements the [Oklch and Oklab](https://bottosson.github.io/posts/oklab/) color spaces for Elm.

It allows conversion from and to the `Color` type in [avh4/elm-color](https://github.com/avh4/elm-color).

See some end-to-end example code in the `examples/` folder.

## Overview

```elm
import Color exposing (Color)
import Color.Oklch

myColor : Color
myColor =
    Color.Oklch.oklch 1 0.1 0.75
        |> Color.Oklch.toColor
```
