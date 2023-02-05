module Color.Oklab exposing
    ( Oklab
    , oklab, oklaba
    , fromColor, toColor
    , fromLinearRGB, toLinearRGB
    )

{-|


# Types

@docs Oklab


# Creating colors

@docs oklab, oklaba


# Conversions

@docs fromColor, toColor
@docs fromLinearRGB, toLinearRGB

-}

import Color exposing (Color)
import Color.LinearRGB exposing (LinearRGB)


type alias Oklab =
    { lightness : Float
    , a : Float
    , b : Float
    , alpha : Float
    }


{-| Convert a color from [`Color`](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0/Color#Color) (sRGB) to Oklab.
-}
fromColor : Color -> Oklab
fromColor color =
    Color.LinearRGB.fromColor color
        |> fromLinearRGB


{-| Convert a color from Oklab to [`Color`](https://package.elm-lang.org/packages/avh4/elm-color/1.0.0/Color#Color) (sRGB).
-}
toColor : Oklab -> Color
toColor color =
    toLinearRGB color
        |> Color.LinearRGB.toColor


oklab : Float -> Float -> Float -> Oklab
oklab lightness a b =
    { lightness = lightness
    , a = a
    , b = b
    , alpha = 1
    }


oklaba : Float -> Float -> Float -> Float -> Oklab
oklaba lightness a b alpha =
    { lightness = lightness
    , a = a
    , b = b
    , alpha = alpha
    }


{-| Converts a color from linear RGB (red, green, blue) to Oklab.

The components should be between 0.0 and 1.0 (inclusive).

-}
fromLinearRGB : LinearRGB -> Oklab
fromLinearRGB { linearRed, linearGreen, linearBlue, alpha } =
    -- https://bottosson.github.io/posts/oklab/
    let
        l : Float
        l =
            0.4122214708 * linearRed + 0.5363325363 * linearGreen + 0.0514459929 * linearBlue

        m : Float
        m =
            0.2119034982 * linearRed + 0.6806995451 * linearGreen + 0.1073969566 * linearBlue

        s : Float
        s =
            0.0883024619 * linearRed + 0.2817188376 * linearGreen + 0.6299787005 * linearBlue

        l_ : Float
        l_ =
            l ^ (1 / 3)

        m_ : Float
        m_ =
            m ^ (1 / 3)

        s_ : Float
        s_ =
            s ^ (1 / 3)
    in
    { lightness = 0.2104542553 * l_ + 0.793617785 * m_ - 0.0040720468 * s_
    , a = 1.9779984951 * l_ - 2.428592205 * m_ + 0.4505937099 * s_
    , b = 0.0259040371 * l_ + 0.7827717662 * m_ - 0.808675766 * s_
    , alpha = alpha
    }


{-| Converts a color from Oklab to linear RGB (red, green, blue).
-}
toLinearRGB : Oklab -> LinearRGB
toLinearRGB { lightness, a, b, alpha } =
    let
        l_ : Float
        l_ =
            lightness + 0.3963377774 * a + 0.2158037573 * b

        m_ : Float
        m_ =
            lightness - 0.1055613458 * a - 0.0638541728 * b

        s_ : Float
        s_ =
            lightness - 0.0894841775 * a - 1.291485548 * b

        lOut : Float
        lOut =
            l_ * l_ * l_

        m : Float
        m =
            m_ * m_ * m_

        s : Float
        s =
            s_ * s_ * s_
    in
    { linearRed = 4.0767416621 * lOut - 3.3077115913 * m + 0.2309699292 * s
    , linearBlue = -1.2684380046 * lOut + 2.6097574011 * m - 0.3413193965 * s
    , linearGreen = -0.0041960863 * lOut - 0.7034186147 * m + 1.707614701 * s
    , alpha = alpha
    }
