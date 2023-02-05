module ColorPicker exposing (main)

import Browser
import Color exposing (Color)
import Color.LinearRGB exposing (LinearRGB)
import Color.Oklab exposing (Oklab)
import Color.Oklch exposing (Oklch)
import Element exposing (Attribute, Element, Length, alignRight, centerY, column, el, fill, height, padding, px, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes


type alias Float3 =
    ( Float, Float, Float )


type alias String3 =
    ( String, String, String )


type alias Model =
    { sRGB : String3 -- RGB
    , linearRGB : String3 -- RGB
    , oklab : String3 -- Lab
    , oklch : String3 -- Lch
    , paletteCount : String
    }


type Msg
    = FromSRGB String3
    | FromLinearRGB String3
    | FromOklab String3
    | FromOklch String3
    | PaletteCount String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = Element.layout layoutStyle << view
        , update = update
        }


layoutStyle : List (Attribute Msg)
layoutStyle =
    [ width fill
    , height fill
    , Background.color colors.background
    , Font.color colors.foreground
    , padding 10
    ]


colors : { foreground : Element.Color, background : Element.Color }
colors =
    { foreground = Element.rgb 0.8 0.8 0.8
    , background = Element.rgb 0.15 0.15 0.15
    }


init : Model
init =
    linearToModel "10" (Color.LinearRGB.linearRgb 0 0 0)


linearToModel : String -> LinearRGB -> Model
linearToModel paletteCount linearRGB =
    let
        oklab : Oklab
        oklab =
            Color.Oklab.fromLinearRGB linearRGB
    in
    { sRGB = colorToStrings <| Color.LinearRGB.toColor linearRGB
    , linearRGB = linearRGBToStrings <| linearRGB
    , oklab = oklabToStrings <| oklab
    , oklch = oklchToStrings <| Color.Oklch.fromOklab oklab
    , paletteCount = paletteCount
    }


colorToStrings : Color -> String3
colorToStrings color =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    toStrings red green blue


linearRGBToStrings : LinearRGB -> String3
linearRGBToStrings { linearRed, linearGreen, linearBlue } =
    toStrings linearRed linearGreen linearBlue


oklabToStrings : Oklab -> String3
oklabToStrings { lightness, a, b } =
    toStrings lightness a b


oklchToStrings : Oklch -> String3
oklchToStrings { lightness, chroma, hue } =
    toStrings lightness chroma hue


from : String3 -> (Float3 -> LinearRGB) -> Model -> Model
from toParse toLinear model =
    case parse toParse of
        Nothing ->
            model

        Just parsed ->
            linearToModel model.paletteCount (toLinear parsed)


fromSRGB : String3 -> Model -> Model
fromSRGB sRGBStrings model =
    let
        newModel : Model
        newModel =
            from sRGBStrings
                (\( r, g, b ) ->
                    Color.rgb r g b
                        |> Color.LinearRGB.fromColor
                )
                model
    in
    { newModel | sRGB = sRGBStrings }


fromLinearRGB : String3 -> Model -> Model
fromLinearRGB linearRGBStrings model =
    let
        newModel : Model
        newModel =
            from linearRGBStrings
                (\( lr, lg, lb ) ->
                    Color.LinearRGB.linearRgb lr lg lb
                )
                model
    in
    { newModel | linearRGB = linearRGBStrings }


fromOklab : String3 -> Model -> Model
fromOklab oklabStrings model =
    let
        newModel : Model
        newModel =
            from oklabStrings
                (\( l, a, b ) ->
                    Color.Oklab.oklab l a b
                        |> Color.Oklab.toLinearRGB
                )
                model
    in
    { newModel | oklab = oklabStrings }


fromOklch : String3 -> Model -> Model
fromOklch oklchStrings model =
    let
        newModel : Model
        newModel =
            from oklchStrings
                (\( l, c, h ) ->
                    Color.Oklch.oklch l c h
                        |> Color.Oklch.toOklab
                        |> Color.Oklab.toLinearRGB
                )
                model
    in
    { newModel | oklch = oklchStrings }


toStrings : Float -> Float -> Float -> String3
toStrings a b c =
    ( String.fromFloat a
    , String.fromFloat b
    , String.fromFloat c
    )


parse : String3 -> Maybe ( Float, Float, Float )
parse ( sr, sg, sb ) =
    Maybe.map3
        (\r g b -> ( r, g, b ))
        (String.toFloat sr)
        (String.toFloat sg)
        (String.toFloat sb)


view : Model -> Element Msg
view { sRGB, linearRGB, oklab, oklch, paletteCount } =
    let
        triples :
            List
                { space : String
                , labels : String3
                , toMsg : String3 -> Msg
                , styles : List String
                , value : String3
                }
        triples =
            [ { space = "sRGB"
              , labels = ( "R", "G", "B" )
              , toMsg = FromSRGB
              , styles = [ toStyles2 sRGB (\( r, g, b ) -> Color.rgb r g b) ]
              , value = sRGB
              }
            , { space = "linear RGB"
              , labels = ( "R", "G", "B" )
              , toMsg = FromLinearRGB
              , styles =
                    [ toStyles2 linearRGB
                        (\( lr, lg, lb ) ->
                            Color.LinearRGB.linearRgb lr lg lb
                                |> Color.LinearRGB.toColor
                        )
                    ]
              , value = linearRGB
              }
            , { space = "Oklab"
              , labels = ( "L", "a", "b" )
              , toMsg = FromOklab
              , styles =
                    [ toStyles "oklab" (\( l, a, b ) -> [ pc l, float a, float b ]) oklab
                    , toStyles2 oklab <|
                        \( l, a, b ) ->
                            Color.Oklab.oklab l a b
                                |> Color.Oklab.toColor
                    ]
              , value = oklab
              }
            , { space = "Oklch"
              , labels = ( "L", "C", "H" )
              , toMsg = FromOklch
              , styles =
                    [ toStyles "oklch" (\( l, c, h ) -> [ pc l, float c, float h ]) oklch
                    , toStyles2 oklch <|
                        \( l, c, h ) ->
                            Color.Oklch.oklch l c h
                                |> Color.Oklch.toColor
                    ]
              , value = oklch
              }
            ]
    in
    column [ spacing 10 ]
        [ List.map viewTriple triples
            |> wrappedRow [ spacing 10 ]
        , viewPalette paletteCount oklch
        ]


toStyles2 : String3 -> (( Float, Float, Float ) -> Color.Color) -> String
toStyles2 toParse conversion =
    toParse
        |> parse
        |> Maybe.withDefault ( 0, 0, 0 )
        |> conversion
        |> Color.toCssString


viewPalette : String -> String3 -> Element Msg
viewPalette paletteCount oklch =
    let
        paletteCountInt : Int
        paletteCountInt =
            String.toInt paletteCount
                |> Maybe.withDefault 10
    in
    column
        [ Border.width 1
        , Border.rounded 10
        , padding 10
        , spacing 10
        , width <| Element.minimum 300 fill
        ]
        [ text "Palette"
        , input []
            { label = Input.labelLeft [] <| text "Size"
            , onChange = PaletteCount
            , placeholder = Just <| Input.placeholder [] <| text "10"
            , text = paletteCount
            }
        , case parse oklch of
            Just ( lightness, chroma, _ ) ->
                List.range 1 paletteCountInt
                    |> List.map
                        (\i ->
                            let
                                hue =
                                    toFloat i / toFloat paletteCountInt

                                color =
                                    Color.Oklch.oklch lightness chroma hue
                                        |> Color.Oklch.toColor
                            in
                            viewColor { width = px 40, height = px 40 } [ Color.toCssString color ]
                        )
                    |> wrappedRow
                        [ spacing 10
                        , width fill
                        ]

            Nothing ->
                Element.none
        ]


input :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
input attrs =
    Input.text
        (Background.color colors.background
            :: Font.color colors.foreground
            :: Border.rounded 10
            :: attrs
        )


toStyles : String -> (( Float, Float, Float ) -> List String) -> String3 -> String
toStyles function toList value =
    let
        parsed : ( Float, Float, Float )
        parsed =
            Maybe.withDefault ( 0, 0, 0 ) <| parse value
    in
    function ++ "(" ++ String.join " " (toList parsed) ++ ")"


pc : Float -> String
pc v =
    String.fromFloat (v * 100) ++ "%"


float : Float -> String
float v =
    String.fromFloat v


viewTriple :
    { space : String
    , labels : ( String, String, String )
    , toMsg : String3 -> Msg
    , styles : List String
    , value : String3
    }
    -> Element Msg
viewTriple { space, labels, toMsg, styles, value } =
    let
        ( label0, label1, label2 ) =
            labels

        ( value0, value1, value2 ) =
            value

        line : String -> String -> (String -> String3) -> ( Element Msg, Element Msg )
        line l v f =
            ( el [ centerY ] <| text l
            , input [ width fill ]
                { label = Input.labelHidden l
                , onChange = \newValue -> toMsg (f newValue)
                , text = v
                , placeholder = Nothing
                }
            )
    in
    column
        [ Border.width 1
        , Border.rounded 10
        , padding 10
        , spacing 10
        , width <| px 300
        ]
        [ table
            [ width fill
            , spacing 10
            ]
            { data =
                [ line label0 value0 (\v -> ( v, value1, value2 ))
                , line label1 value1 (\v -> ( value0, v, value2 ))
                , line label2 value2 (\v -> ( value0, value1, v ))
                ]
            , columns =
                [ { header = Element.none
                  , width = shrink
                  , view = Tuple.first
                  }
                , { header = el [ alignRight ] <| text space
                  , width = fill
                  , view = Tuple.second
                  }
                ]
            }
        , viewColor { width = fill, height = px 40 } styles
        ]


viewColor : { height : Length, width : Length } -> List String -> Element Msg
viewColor size styles =
    el
        ([ width size.width
         , height size.height
         , Border.width 1
         , Border.rounded 10
         ]
            ++ List.map
                (\s ->
                    Element.htmlAttribute (Html.Attributes.style "background" s)
                )
                styles
        )
        Element.none


update : Msg -> Model -> Model
update msg model =
    case msg of
        FromSRGB value ->
            fromSRGB value model

        FromLinearRGB value ->
            fromLinearRGB value model

        FromOklab value ->
            fromOklab value model

        FromOklch value ->
            fromOklch value model

        PaletteCount paletteCount ->
            { model | paletteCount = paletteCount }
