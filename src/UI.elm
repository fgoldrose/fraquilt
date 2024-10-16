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
        [ HA.class "flex flex-col items-center gap-2"
        ]
        (Html.span
            [ HA.class "font-semibold" ]
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
        [ HA.class "flex items-center gap-1"
        ]
        [ Html.label
            [ HA.for id
            , HA.class "w-7 text-right"
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
