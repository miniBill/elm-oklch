module Bijection exposing (suite)

import Color exposing (Color)
import Color.LinearRGB
import Color.Oklab
import Color.Oklch
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Conversion functions are inverse of each other"
        [ describe "Linear RGB"
            [ fromToColor Color.LinearRGB.fromColor Color.LinearRGB.toColor <|
                \{ linearRed, linearGreen, linearBlue } -> ( linearRed, linearGreen, linearBlue )
            ]
        , describe "Oklab"
            [ fromToColor Color.Oklab.fromColor Color.Oklab.toColor <|
                \{ lightness, a, b } -> ( lightness, a, b )
            ]
        , describe "Oklch"
            [ fromToColor Color.Oklch.fromColor Color.Oklch.toColor <|
                \{ lightness, chroma, hue } -> ( lightness, chroma, hue )
            ]
        ]


fromToColor : (Color -> color) -> (color -> Color) -> (color -> ( Float, Float, Float )) -> Test
fromToColor fromColor toColor toFloats =
    describe "fromColor/toColor are inverse of each other"
        [ fuzz fuzzColor "toColor >> fromColor === identity" <|
            \c ->
                let
                    color : color
                    color =
                        fromColor c
                in
                color
                    |> toColor
                    |> fromColor
                    |> toFloats
                    |> almostEqual (toFloats color)
        , fuzz fuzzColor "fromColor >> toColor === identity" <|
            \color ->
                color
                    |> fromColor
                    |> toColor
                    |> colorToFloats
                    |> almostEqual (colorToFloats color)
        ]


colorToFloats : Color -> ( Float, Float, Float )
colorToFloats color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    ( red, green, blue )


almostEqual : ( Float, Float, Float ) -> ( Float, Float, Float ) -> Expectation
almostEqual expected actual =
    let
        expectations : List (( Float, Float, Float ) -> Expectation)
        expectations =
            [ \( a, _, _ ) -> a
            , \( _, a, _ ) -> a
            , \( _, _, a ) -> a
            ]
                |> List.map (\f -> Expect.within (Expect.Absolute 0.00001) (f expected) << f)
    in
    Expect.all expectations actual


fuzzColor : Fuzzer Color
fuzzColor =
    Fuzz.map3 Color.rgb
        (Fuzz.floatRange 0 1)
        (Fuzz.floatRange 0 1)
        (Fuzz.floatRange 0 1)
