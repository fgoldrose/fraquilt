module Colors exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode


type alias Color =
    -- hex
    String


type alias InitialVariables =
    Array Color


set : Int -> Color -> InitialVariables -> InitialVariables
set index color initialVariables =
    Array.set index color initialVariables


count : InitialVariables -> Int
count initialVariables =
    Array.length initialVariables


addN : Int -> InitialVariables -> InitialVariables
addN n initialVariables =
    Array.append initialVariables
        (Array.repeat n "#000000")


removeN : Int -> InitialVariables -> InitialVariables
removeN n initialVariables =
    Array.slice 0 (Array.length initialVariables - n) initialVariables


init3 : InitialVariables
init3 =
    Array.fromList [ "#ffffff", "#808080", "#000000" ]


encode : InitialVariables -> Encode.Value
encode initialVariables =
    Encode.array
        Encode.string
        initialVariables


toUrlString : InitialVariables -> String
toUrlString initialVariables =
    String.join "," (Array.toList initialVariables)


fromUrlString : String -> InitialVariables
fromUrlString str =
    str
        |> String.split ","
        |> Array.fromList


view : (Int -> String -> msg) -> InitialVariables -> Html msg
view updateColorMsg initialVars =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "5px"
        ]
        (List.indexedMap
            (\index initVar ->
                Html.input
                    [ HA.type_ "color"
                    , HA.id ("initVar-" ++ String.fromInt index)
                    , HA.value initVar
                    , HE.onInput (updateColorMsg index)
                    ]
                    []
            )
            (Array.toList initialVars)
        )


readOnlyView : InitialVariables -> Html msg
readOnlyView initialVars =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "5px"
        ]
        (List.indexedMap
            (\index initVar ->
                Html.input
                    [ HA.type_ "color"
                    , HA.id ("initVar-" ++ String.fromInt index)
                    , HA.value initVar
                    , HA.disabled True
                    , HA.readonly True
                    ]
                    []
            )
            (Array.toList initialVars)
        )
