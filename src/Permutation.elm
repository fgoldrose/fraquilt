module Permutation exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import List.Extra
import Messages exposing (Msg(..))
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (Mode(..), Quadrant(..), SelectionState(..), getSelectedForQuadrant)


type alias Permutation =
    List Int


getNumVars : Permutation -> Int
getNumVars colorArray =
    List.length colorArray


setNewLine : Int -> Int -> Permutation -> Permutation
setNewLine fromIndex toIndex colorArray =
    List.Extra.setAt fromIndex toIndex colorArray


addN : Int -> Permutation -> Permutation
addN n permutation =
    let
        startIndex =
            getNumVars permutation
    in
    List.append permutation (List.range startIndex (startIndex + n - 1))


removeN : Int -> Permutation -> Permutation
removeN n permutation =
    let
        numVars =
            getNumVars permutation

        badIndexes =
            List.range (numVars - n) numVars

        replaceIndexes =
            badIndexes
                |> List.filterMap
                    (\index ->
                        List.Extra.getAt index permutation
                            |> Maybe.map (Tuple.pair index)
                    )
                |> Dict.fromList
    in
    permutation
        |> List.take (numVars - n)
        |> List.map
            (\index ->
                Dict.get index replaceIndexes |> Maybe.withDefault index
            )


view : Permutation -> Quadrant -> Mode -> SelectionState -> Html Msg
view permutation quadrant mode selectionState =
    let
        numVars =
            getNumVars permutation

        listRange =
            List.range 0 (numVars - 1)

        pixelSize =
            ((30 * numVars) |> String.fromInt) ++ "px"
    in
    Html.div
        [ HA.style "background-color" "rgb(200, 200, 200)"
        , HA.style "border-radius" "6%"
        , HA.style "padding" "20px 0"
        ]
        [ Html.div
            [ HA.style "position" "relative"
            , HA.style "width" "100px"
            , HA.style "height" pixelSize
            ]
            (lines permutation
                :: List.map
                    (dot
                        { side = Left
                        , totalVars = numVars
                        , quadrant = quadrant
                        , selectionState = selectionState
                        , mode = mode
                        }
                    )
                    listRange
                ++ List.map
                    (dot
                        { side = Right
                        , totalVars = numVars
                        , quadrant = quadrant
                        , selectionState = selectionState
                        , mode = mode
                        }
                    )
                    listRange
            )
        ]


getTopPositionForIndex : { totalVars : Int, index : Int } -> String
getTopPositionForIndex { index, totalVars } =
    ((toFloat index / toFloat (totalVars - 1))
        * 100
    )
        |> String.fromFloat
        |> (\x -> x ++ "%")


type Side
    = Left
    | Right


dot :
    { side : Side
    , totalVars : Int
    , quadrant : Quadrant
    , selectionState : SelectionState
    , mode : Mode
    }
    -> Int
    -> Html Msg
dot { side, totalVars, quadrant, selectionState, mode } index =
    let
        maybeSelectedIndex =
            getSelectedForQuadrant quadrant selectionState

        ( leftModeStyles, rightModeStyles ) =
            case mode of
                Permutation ->
                    ( case maybeSelectedIndex of
                        Just selectedIndex ->
                            if selectedIndex == index then
                                [ HA.style "background-color" "rgb(0, 0, 255)"
                                , HA.style "outline" "2px solid rgba(0, 100, 255, 0.5)"
                                , HE.onClick CancelSelection
                                ]

                            else
                                [ HA.style "background-color" "rgb(0, 100, 255)"
                                , HE.onClick (EndSelection index)
                                ]

                        Nothing ->
                            [ if selectionState == NoneSelected then
                                HA.style "background-color" "rgb(0, 100, 255)"

                              else
                                HA.style "background-color" "black"
                            , HE.onClick (StartSelection quadrant index)
                            ]
                    , [ HA.style "background-color" "black" ]
                    )

                Free ->
                    case maybeSelectedIndex of
                        Just selectedIndex ->
                            ( if selectedIndex == index then
                                [ HA.style "background-color" "rgb(0, 0, 255)"
                                , HA.style "outline" "2px solid rgba(0, 100, 255, 0.5)"
                                , HE.onClick CancelSelection
                                ]

                              else
                                [ if selectionState == NoneSelected then
                                    HA.style "background-color" "rgb(0, 100, 255)"

                                  else
                                    HA.style "background-color" "black"
                                , HE.onClick (StartSelection quadrant index)
                                ]
                            , [ HA.style "cursor" "pointer"
                              , HE.onClick (EndSelection index)
                              , HA.style "background-color" "rgb(0, 100, 255)"
                              ]
                            )

                        Nothing ->
                            ( [ if selectionState == NoneSelected then
                                    HA.style "background-color" "rgb(0, 100, 255)"

                                else
                                    HA.style "background-color" "black"
                              , HE.onClick (StartSelection quadrant index)
                              ]
                            , [ HA.style "background-color" "black" ]
                            )

        leftStyles =
            [ HA.style "left" "0"
            , HA.style "transform" "translate(-50%, -50%)"
            , HA.style "cursor" "pointer"
            , HA.style "filter" "drop-shadow(5px 5px 2px rgba(0, 0, 0, 0.5))"
            , HA.style "transition" "background-color 0.2s ease-in"
            ]
                ++ leftModeStyles

        rightStyles =
            [ HA.style "right" "0"
            , HA.style "transform" "translate(50%, -50%)"
            , HA.style "filter" "drop-shadow(5px 5px 2px rgba(0, 0, 0, 0.5))"
            , HA.style "transition" "background-color 0.2s ease-in"
            ]
                ++ rightModeStyles
    in
    Html.div
        ([ HA.style "width" "20px"
         , HA.style "height" "20px"
         , HA.style "border-radius" "100%"
         , HA.style "position" "absolute"
         , HA.style "top"
            (getTopPositionForIndex
                { index = index, totalVars = totalVars }
            )
         ]
            ++ (case side of
                    Left ->
                        leftStyles

                    Right ->
                        rightStyles
               )
        )
        []


lines : Permutation -> Html Msg
lines permutation =
    let
        numVars =
            List.length permutation
    in
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "inset" "0"
        ]
        [ Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , HA.style "overflow" "visible"
            ]
            (List.filterMap
                (\fromIndex ->
                    List.Extra.getAt fromIndex permutation
                        |> Maybe.map
                            (\toIndex ->
                                line
                                    { totalVars = numVars
                                    , fromIndex = fromIndex
                                    , toIndex = toIndex
                                    }
                            )
                )
                (List.range 0 (numVars - 1))
            )
        ]


line : { totalVars : Int, fromIndex : Int, toIndex : Int } -> Svg msg
line { totalVars, fromIndex, toIndex } =
    let
        y1 =
            getTopPositionForIndex
                { index = fromIndex
                , totalVars = totalVars
                }

        y2 =
            getTopPositionForIndex
                { index = toIndex
                , totalVars = totalVars
                }
    in
    Svg.line
        [ Svg.Attributes.y1 y1
        , Svg.Attributes.y2 y2
        , Svg.Attributes.x1 "0%"
        , Svg.Attributes.x2 "100%"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "2"
        ]
        []


encode : Permutation -> Encode.Value
encode adjustments =
    Encode.list Encode.int adjustments


random : Int -> Random.Generator Permutation
random numVars =
    Random.list numVars (Random.int 0 (numVars - 1))


randomPermutation : Int -> Random.Generator Permutation
randomPermutation numVars =
    -- Version that produces a one to one permutation
    Random.List.shuffle (List.range 0 (numVars - 1))
