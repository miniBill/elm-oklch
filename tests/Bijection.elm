module Bijection exposing (suite)

import Color exposing (Color)
import Color.LinearRGB exposing (LinearRGB)
import Color.Oklab
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Conversion functions are inverse of each other"
        [ describe "Linear RGB"
            [ fromToColor Color.LinearRGB.fromColor Color.LinearRGB.toColor fuzzLinearRGB <| \{ linearRed, linearGreen, linearBlue } -> ( linearRed, linearGreen, linearBlue )
            ]
        , describe "Oklab"
            [ fromToColor Color.Oklab.fromColor Color.Oklab.toColor fuzzOklab <| \{ lightness, a, b } -> ( lightness, a, b )
            ]
        ]


fromToColor : (Color -> color) -> (color -> Color) -> Fuzzer color -> (color -> ( Float, Float, Float )) -> Test
fromToColor fromColor toColor fuzzer toFloats =
    describe "fromColor/toColor are inverse of each other"
        [ fuzz fuzzer "toColor >> fromColor === identity" <|
            \color ->
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
                |> List.map (\f -> Expect.within (Expect.Absolute 0.0001) (f expected) << f)
    in
    Expect.all expectations actual



-- Fuzzers --


fuzzColor : Fuzzer Color
fuzzColor =
    fuzzFloat3 Color.rgb


fuzzLinearRGB : Fuzzer LinearRGB
fuzzLinearRGB =
    fuzzFloat3 Color.LinearRGB.linearRgb


fuzzOklab : Fuzzer Color.Oklab.Oklab
fuzzOklab =
    fuzzFloat3 Color.Oklab.oklab


fuzzFloat3 : (Float -> Float -> Float -> color) -> Fuzzer color
fuzzFloat3 f =
    Fuzz.map3 f
        (Fuzz.floatRange 0 1)
        (Fuzz.floatRange 0 1)
        (Fuzz.floatRange 0 1)
