module Theme exposing (colors, input, padding, rythm, spacing)

import Element exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


colors :
    { background : Color
    , foreground : Color
    }
colors =
    { foreground = Element.rgb 0.8 0.8 0.8
    , background = Element.rgb 0.15 0.15 0.15
    }


spacing : Attribute msg
spacing =
    Element.spacing rythm


padding : Attribute msg
padding =
    Element.padding rythm


rythm : number
rythm =
    10


input :
    List (Attribute msg)
    ->
        { label : Input.Label msg
        , onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        }
    -> Element msg
input attrs =
    Input.text
        (Background.color colors.background
            :: Font.color colors.foreground
            :: Border.rounded rythm
            :: attrs
        )
