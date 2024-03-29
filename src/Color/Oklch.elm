module Color.Oklch exposing
    ( Oklch
    , oklch, oklcha
    , fromColor, toColor
    , fromOklab, toOklab
    )

{-|


# Types

@docs Oklch


# Creating colors

@docs oklch, oklcha


# Conversions

@docs fromColor, toColor
@docs fromOklab, toOklab

-}

import Color exposing (Color)
import Color.Oklab exposing (Oklab)


{-| A color in the Oklch space.

  - `lightness` ranges from 0.0 to 1.0 (inclusive),
  - `chroma` ranges from 0.0 to approximatively 0.37 (inclusive),
  - `hue` ranges from 0.0 to 1.0 (inclusive),
  - `alpha` ranges from 0.0 to 1.0 (inclusive).

-}
type alias Oklch =
    { lightness : Float
    , chroma : Float
    , hue : Float
    , alpha : Float
    }


{-| Builds a color from its lightness, chroma and hue components.

  - `lightness` ranges from 0.0 to 1.0 (inclusive),
  - `chroma` ranges from 0.0 to approximatively 0.37 (inclusive),
  - `hue` ranges from 0.0 to 1.0 (inclusive),
  - `alpha` ranges from 0.0 to 1.0.

-}
oklch : Float -> Float -> Float -> Oklch
oklch lightness chroma hue =
    { lightness = lightness
    , chroma = chroma
    , hue = hue
    , alpha = 1
    }


{-| Builds a color from its lightness, chroma, hue and alpha components.

  - `lightness` ranges from 0.0 to 1.0 (inclusive),
  - `chroma` ranges from 0.0 to approximatively 0.37 (inclusive),
  - `hue` ranges from 0.0 to 1.0 (inclusive),
  - `alpha` ranges from 0.0 to 1.0.

-}
oklcha : Float -> Float -> Float -> Float -> Oklch
oklcha lightness chroma hue alpha =
    { lightness = lightness
    , chroma = chroma
    , hue = hue
    , alpha = alpha
    }


{-| Convert from Oklab to Oklch.
-}
fromOklab : Oklab -> Oklch
fromOklab { lightness, a, b, alpha } =
    let
        chroma : Float
        chroma =
            sqrt (a * a + b * b)

        hue_ : Float
        hue_ =
            atan2 b a / (pi * 2)

        hue : Float
        hue =
            if hue_ < 0 then
                1 + hue_

            else
                hue_
    in
    { lightness = lightness
    , chroma = chroma
    , hue = hue
    , alpha = alpha
    }


{-| Convert from Oklch to Oklab.
-}
toOklab : Oklch -> Oklab
toOklab { lightness, chroma, hue, alpha } =
    let
        a : Float
        a =
            chroma * cos (hue * 2 * pi)

        b : Float
        b =
            chroma * sin (hue * 2 * pi)
    in
    { lightness = lightness
    , a = a
    , b = b
    , alpha = alpha
    }


{-| Convert a color from [`Color`](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0/Color#Color) (sRGB) to Oklch.
-}
fromColor : Color -> Oklch
fromColor color =
    Color.Oklab.fromColor color
        |> fromOklab


{-| Convert a color from Oklch to [`Color`](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0/Color#Color) (sRGB).
-}
toColor : Oklch -> Color
toColor color =
    toOklab color
        |> Color.Oklab.toColor
