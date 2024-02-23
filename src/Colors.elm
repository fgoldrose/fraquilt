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


view : (Int -> String -> msg) -> InitialVariables -> List (Html msg)
view updateColorMsg initialVars =
    List.indexedMap
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
