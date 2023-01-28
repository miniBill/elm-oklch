module ColorPicker exposing (main)

import Browser
import Element exposing (Attribute, Element, Length, alignRight, centerY, column, el, fill, height, px, shrink, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Theme


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
    , Background.color Theme.colors.background
    , Font.color Theme.colors.foreground
    , Theme.padding
    ]


init : Model
init =
    linearToModel "10" ( 0, 0, 0 )


linearToModel : String -> Float3 -> Model
linearToModel paletteCount linearRGB =
    let
        oklab : Float3
        oklab =
            linearToOklab linearRGB
    in
    { sRGB = transform linearToSRGB linearRGB
    , linearRGB = toString3 linearRGB
    , oklab = toString3 oklab
    , oklch = transform3 oklabToOklch oklab
    , paletteCount = paletteCount
    }


from : String3 -> (Float3 -> Float3) -> Model -> Model
from input toLinear model =
    case parse input of
        Nothing ->
            model

        Just parsed ->
            linearToModel model.paletteCount (toLinear parsed)


fromSRGB : String3 -> Model -> Model
fromSRGB sRGBStrings model =
    let
        newModel : Model
        newModel =
            from sRGBStrings (triple sRGBToLinear) model
    in
    { newModel | sRGB = sRGBStrings }


fromLinearRGB : String3 -> Model -> Model
fromLinearRGB linearRGBStrings model =
    let
        newModel : Model
        newModel =
            from linearRGBStrings identity model
    in
    { newModel | linearRGB = linearRGBStrings }


fromOklab : String3 -> Model -> Model
fromOklab oklabStrings model =
    let
        newModel : Model
        newModel =
            from oklabStrings oklabToLinear model
    in
    { newModel | oklab = oklabStrings }


fromOklch : String3 -> Model -> Model
fromOklch oklchStrings model =
    let
        newModel : Model
        newModel =
            from oklchStrings (oklchToOklab >> oklabToLinear) model
    in
    { newModel | oklch = oklchStrings }


transform : (Float -> Float) -> Float3 -> String3
transform transformation value =
    toString3 <| triple transformation value


transform3 : (Float3 -> Float3) -> Float3 -> String3
transform3 transformation value =
    toString3 <| transformation value


toString3 : Float3 -> String3
toString3 =
    triple String.fromFloat


parse : String3 -> Maybe ( Float, Float, Float )
parse value =
    case triple String.toFloat value of
        ( Just r, Just g, Just b ) ->
            Just ( r, g, b )

        _ ->
            Nothing


triple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
triple f ( r, g, b ) =
    ( f r, f g, f b )


sRGBToLinear : Float -> Float
sRGBToLinear s =
    -- Higher precision constant from https://entropymine.com/imageworsener/srgbformula/
    if s <= 0.0404482362771082 then
        s / 12.92

    else
        ((s + 0.055) / 1.055) ^ 2.4


linearToSRGB : Float -> Float
linearToSRGB l =
    -- Higher precision constant from https://entropymine.com/imageworsener/srgbformula/
    if l <= 0.00313066844250063 then
        l * 12.92

    else
        1.055 * l ^ (1 / 2.4) - 0.055


linearToOklab : Float3 -> Float3
linearToOklab ( r, g, b ) =
    -- https://bottosson.github.io/posts/oklab/
    let
        l : Float
        l =
            0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b

        m : Float
        m =
            0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b

        s : Float
        s =
            0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b

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
    ( 0.2104542553 * l_ + 0.793617785 * m_ - 0.0040720468 * s_
    , 1.9779984951 * l_ - 2.428592205 * m_ + 0.4505937099 * s_
    , 0.0259040371 * l_ + 0.7827717662 * m_ - 0.808675766 * s_
    )


oklabToLinear : Float3 -> Float3
oklabToLinear ( l, a, b ) =
    let
        l_ : Float
        l_ =
            l + 0.3963377774 * a + 0.2158037573 * b

        m_ : Float
        m_ =
            l - 0.1055613458 * a - 0.0638541728 * b

        s_ : Float
        s_ =
            l - 0.0894841775 * a - 1.291485548 * b

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
    ( 4.0767416621 * lOut - 3.3077115913 * m + 0.2309699292 * s
    , -1.2684380046 * lOut + 2.6097574011 * m - 0.3413193965 * s
    , -0.0041960863 * lOut - 0.7034186147 * m + 1.707614701 * s
    )


oklabToOklch : Float3 -> Float3
oklabToOklch ( l, a, b ) =
    let
        c : Float
        c =
            sqrt (a * a + b * b)

        h_ =
            atan2 b a * 180 / pi

        h : Float
        h =
            if h_ < 0 then
                360 + h_

            else
                h_
    in
    ( l, c, h )


oklchToOklab : Float3 -> Float3
oklchToOklab ( l, c, h ) =
    let
        a : Float
        a =
            c * cos (degrees h)

        b : Float
        b =
            c * sin (degrees h)
    in
    ( l, a, b )


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
              , styles = [ toStyles "rgb" (\( r, g, b ) -> [ pc r, pc g, pc b ]) sRGB ]
              , value = sRGB
              }
            , { space = "linear RGB"
              , labels = ( "R", "G", "B" )
              , toMsg = FromLinearRGB
              , styles = [ toStyles "rgb" (\( r, g, b ) -> [ pc r, pc g, pc b ]) {- Not a typo -} sRGB ]
              , value = linearRGB
              }
            , { space = "Oklab"
              , labels = ( "L", "a", "b" )
              , toMsg = FromOklab
              , styles =
                    [ toStyles "oklab" (\( l, a, b ) -> [ pc l, float a, float b ]) oklab
                    , toStyles "rgb"
                        (\v ->
                            let
                                ( r, g, b ) =
                                    triple linearToSRGB <| oklabToLinear v
                            in
                            [ pc r, pc g, pc b ]
                        )
                        oklab
                    ]
              , value = oklab
              }
            , { space = "Oklch"
              , labels = ( "L", "C", "H" )
              , toMsg = FromOklch
              , styles =
                    [ toStyles "lklch" (\( l, c, h ) -> [ pc l, float c, float h ]) oklch
                    , toStyles "rgb"
                        (\v ->
                            let
                                ( r, g, b ) =
                                    triple linearToSRGB <| oklabToLinear <| oklchToOklab v
                            in
                            [ pc r, pc g, pc b ]
                        )
                        oklch
                    ]
              , value = oklch
              }
            ]
    in
    column [ Theme.spacing ]
        [ List.map viewTriple triples
            |> wrappedRow [ Theme.spacing ]
        , viewPalette paletteCount oklch
        ]


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
        , Border.rounded Theme.rythm
        , Theme.padding
        , Theme.spacing
        , width <| Element.minimum 300 fill
        ]
        [ text "Palette"
        , Theme.input []
            { label = Input.labelLeft [] <| text "Size"
            , onChange = PaletteCount
            , placeholder = Just <| Input.placeholder [] <| text "10"
            , text = paletteCount
            }
        , case parse oklch of
            Just ( l, c, _ ) ->
                List.range 1 paletteCountInt
                    |> List.map
                        (\i ->
                            let
                                hue =
                                    toFloat i * 360 / toFloat paletteCountInt

                                ( r, g, b ) =
                                    ( l, c, hue )
                                        |> oklchToOklab
                                        |> oklabToLinear
                                        |> triple linearToSRGB
                            in
                            viewColor { width = px 40, height = px 40 } [ "rgb(" ++ String.join " " [ pc r, pc g, pc b ] ++ ")" ]
                        )
                    |> wrappedRow
                        [ Theme.spacing
                        , width fill
                        ]

            Nothing ->
                Element.none
        ]


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
            , Theme.input [ width fill ]
                { label = Input.labelHidden l
                , onChange = \newValue -> toMsg (f newValue)
                , text = v
                , placeholder = Nothing
                }
            )
    in
    column
        [ Border.width 1
        , Border.rounded Theme.rythm
        , Theme.padding
        , Theme.spacing
        , width <| px 300
        ]
        [ table
            [ width fill
            , Theme.spacing
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
         , Border.rounded Theme.rythm
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
