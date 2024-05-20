module UI exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE


pxInt : Int -> String
pxInt i =
    String.fromInt i ++ "px"


pxFloat : Float -> String
pxFloat f =
    String.fromFloat f ++ "px"


sectionWithName : String -> List (Html msg) -> Html msg
sectionWithName labelText elements =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "5px"
        ]
        (Html.span
            [ HA.style "font-weight" "bold" ]
            [ Html.text labelText ]
            :: elements
        )


sliderWithLabel :
    { id : String
    , value : Int
    , msg : String -> msg
    , min : Int
    , max : Int
    }
    -> Html msg
sliderWithLabel { id, value, msg, min, max } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "gap" "5px"
        ]
        [ Html.label
            [ HA.for id
            , HA.style "width" "30px"
            , HA.style "text-align" "right"
            ]
            [ Html.text (String.fromInt value) ]
        , Html.input
            [ HA.id id
            , HA.type_ "range"
            , HA.min (String.fromInt min)
            , HA.max (String.fromInt max)
            , HA.value (String.fromInt value)
            , HE.onInput msg
            ]
            []
        ]
