module Color.LinearRGB exposing
    ( LinearRGB
    , linearRgb, linearRgba
    , fromColor, toColor
    )

{-|


# Types

@docs LinearRGB


# Creating colors

@docs linearRgb, linearRgba


# Conversions

@docs fromColor, toColor

-}

import Color exposing (Color)


{-| Represents a color in the linear RGB space.
-}
type alias LinearRGB =
    { linearRed : Float
    , linearGreen : Float
    , linearBlue : Float
    , alpha : Float
    }


linearRgb : Float -> Float -> Float -> LinearRGB
linearRgb linearRed linearGreen linearBlue =
    { linearRed = linearRed
    , linearGreen = linearGreen
    , linearBlue = linearBlue
    , alpha = 1
    }


linearRgba : Float -> Float -> Float -> Float -> LinearRGB
linearRgba linearRed linearGreen linearBlue alpha =
    { linearRed = linearRed
    , linearGreen = linearGreen
    , linearBlue = linearBlue
    , alpha = alpha
    }


{-| Convert a color from [`Color`](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0/Color#Color) (sRGB) to linear RGB.

All component range from 0.0 to 1.0 (inclusive).

-}
fromColor : Color -> LinearRGB
fromColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    { linearRed = sRGBToLinear red
    , linearGreen = sRGBToLinear green
    , linearBlue = sRGBToLinear blue
    , alpha = alpha
    }


{-| Convert a color from linear RGB to [`Color`](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0/Color#Color) (sRGB).

All component range from 0.0 to 1.0 (inclusive).

-}
toColor : LinearRGB -> Color
toColor { linearRed, linearGreen, linearBlue, alpha } =
    Color.rgba
        (linearToSRGB linearRed)
        (linearToSRGB linearGreen)
        (linearToSRGB linearBlue)
        alpha


{-| Convert one component from sRGB to linear RGB.
-}
sRGBToLinear : Float -> Float
sRGBToLinear s =
    -- Higher precision constant from https://entropymine.com/imageworsener/srgbformula/
    if s <= 0.0404482362771082 then
        s / 12.92

    else
        ((s + 0.055) / 1.055) ^ 2.4


{-| Convert one component from linear RGB to sRGB.
-}
linearToSRGB : Float -> Float
linearToSRGB l =
    -- Higher precision constant from https://entropymine.com/imageworsener/srgbformula/
    if l <= 0.00313066844250063 then
        l * 12.92

    else
        1.055 * l ^ (1 / 2.4) - 0.055
