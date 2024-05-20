module Colors exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import UI exposing (pxFloat, pxInt)


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


view : Float -> Maybe (Int -> String -> msg) -> InitialVariables -> Html msg
view colorSize maybeUpdateColorMsg initialVars =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" (pxFloat (colorSize / 8))
        ]
        (List.indexedMap
            (\index initVar ->
                case maybeUpdateColorMsg of
                    Just updateColorMsg ->
                        Html.input
                            [ HA.type_ "color"
                            , HA.id ("initVar-" ++ String.fromInt index)
                            , HA.value initVar
                            , HA.style "height" (pxFloat colorSize)
                            , HA.style "width" (pxFloat colorSize)
                            , HA.style "padding" "0"
                            , HE.onInput (updateColorMsg index)
                            ]
                            []

                    Nothing ->
                        Html.div
                            [ HA.style "height" (pxFloat colorSize)
                            , HA.style "width" (pxFloat colorSize)
                            , HA.style "background-color" initVar
                            , HA.style "border-radius" "5%"
                            , HA.style "border" "1px solid black"
                            ]
                            []
            )
            (Array.toList initialVars)
        )
