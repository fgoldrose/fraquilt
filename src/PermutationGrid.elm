module PermutationGrid exposing (..)

import Html exposing (Html)
import Html.Attributes as HA
import Json.Encode as Encode
import Permutation exposing (Permutation)
import Random
import Svg exposing (symbol)
import Types exposing (Quadrant(..), SelectionState(..))
import UI exposing (pxFloat, pxInt)


type alias PermutationGrid =
    { tl : Permutation
    , tr : Permutation
    , bl : Permutation
    , br : Permutation
    }


clear : Int -> PermutationGrid
clear numVars =
    { tl = List.range 0 (numVars - 1)
    , tr = List.range 0 (numVars - 1)
    , bl = List.range 0 (numVars - 1)
    , br = List.range 0 (numVars - 1)
    }


endSelection : SelectionState -> Int -> Bool -> PermutationGrid -> PermutationGrid
endSelection selectionState endIndex symmetric settings =
    case selectionState of
        TLSelected startIndex ->
            let
                newPermutation =
                    Permutation.swap startIndex endIndex settings.tl
            in
            { settings
                | tl = newPermutation
                , br =
                    if symmetric then
                        newPermutation

                    else
                        settings.br
            }

        TRSelected startIndex ->
            let
                newPermutation =
                    Permutation.swap startIndex endIndex settings.tr
            in
            { settings
                | tr = newPermutation
                , bl =
                    if symmetric then
                        newPermutation

                    else
                        settings.bl
            }

        BLSelected startIndex ->
            { settings
                | bl = Permutation.swap startIndex endIndex settings.bl
            }

        BRSelected startIndex ->
            { settings
                | br = Permutation.swap startIndex endIndex settings.br
            }

        NoneSelected ->
            settings


random : Int -> Random.Generator PermutationGrid
random numVars =
    Random.map4
        (\tl tr bl br ->
            { tl = tl
            , tr = tr
            , bl = bl
            , br = br
            }
        )
        (Permutation.random numVars)
        (Permutation.random numVars)
        (Permutation.random numVars)
        (Permutation.random numVars)


randomSymmetric : Int -> Random.Generator PermutationGrid
randomSymmetric numVars =
    Random.map2
        (\permutation1 permutation2 ->
            { tl = permutation1
            , tr = permutation2
            , bl = permutation2
            , br = permutation1
            }
        )
        (Permutation.random numVars)
        (Permutation.random numVars)


encode : PermutationGrid -> Encode.Value
encode settings =
    Encode.object
        [ ( "tl", Permutation.encode settings.tl )
        , ( "tr", Permutation.encode settings.tr )
        , ( "bl", Permutation.encode settings.bl )
        , ( "br", Permutation.encode settings.br )
        ]


view :
    { startSelection : Quadrant -> Int -> msg
    , endSelection : Int -> msg
    , cancelSelection : msg
    , selectionState : SelectionState
    , dotPixelSize : Int
    , symmetric : Bool
    }
    -> PermutationGrid
    -> Html msg
view config settings =
    let
        permutationConfig quadrant =
            { permutationSelection = Types.getSelectedForQuadrant quadrant config.selectionState
            , startSelection = config.startSelection quadrant
            , endSelection = config.endSelection
            , cancelSelection = config.cancelSelection
            , dotPixelSize = config.dotPixelSize
            , disabled =
                config.symmetric
                    && (quadrant == BottomLeft || quadrant == BottomRight)
            }

        permWidth =
            config.dotPixelSize * 5 |> pxInt
    in
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" (permWidth ++ " " ++ permWidth)
        , HA.style "grid-gap" (pxFloat (toFloat config.dotPixelSize * 1.5))
        ]
        [ Permutation.view (permutationConfig TopLeft) settings.tl
        , Permutation.view (permutationConfig TopRight) settings.tr
        , Permutation.view (permutationConfig BottomLeft) settings.bl
        , Permutation.view (permutationConfig BottomRight) settings.br
        ]
