module Permutation exposing (..)

import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import List.Extra
import Maybe.Extra
import Random
import Random.List
import Svg exposing (Svg)
import Svg.Attributes exposing (strokeWidth)
import Types exposing (PermutationSelection(..))
import UI exposing (pxFloat, pxInt)


type alias Permutation =
    List Int


getNumVars : Permutation -> Int
getNumVars colorArray =
    List.length colorArray


swap : Int -> Int -> Permutation -> Permutation
swap =
    List.Extra.swapAt


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


type alias Config msg =
    { permutationSelection : PermutationSelection
    , startSelection : Int -> msg
    , endSelection : Int -> msg
    , cancelSelection : msg
    , dotPixelSize : Int
    }


view : Config msg -> Permutation -> Html msg
view config permutation =
    let
        numVars =
            getNumVars permutation

        listRange =
            List.range 0 (numVars - 1)
    in
    Html.div
        [ HA.style "background-color" "rgb(200, 200, 200)"
        , HA.style "border-radius" "6%"
        , HA.style "padding" (pxInt config.dotPixelSize ++ " 0")
        ]
        [ Html.div
            [ HA.style "position" "relative"
            , HA.style "width" (pxInt (config.dotPixelSize * 5))
            , HA.style "height" (pxInt (round (toFloat (config.dotPixelSize * (numVars - 1)) * 2)))
            ]
            (lines (toFloat config.dotPixelSize / 10) permutation
                :: List.map
                    (dot
                        { side = Left
                        , totalVars = numVars
                        , config = config
                        }
                    )
                    listRange
                ++ List.map
                    (dot
                        { side = Right
                        , totalVars = numVars
                        , config = config
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
    , config : Config msg
    }
    -> Int
    -> Html msg
dot { side, totalVars, config } index =
    let
        { permutationSelection, startSelection, endSelection, cancelSelection } =
            config

        ( rightModeStyles, leftModeStyles ) =
            ( case permutationSelection of
                Selected selectedIndex ->
                    if selectedIndex == index then
                        [ HA.style "background-color" "rgb(0, 0, 255)"
                        , HA.style "outline"
                            (pxFloat (toFloat config.dotPixelSize / 10)
                                ++ " solid rgba(0, 100, 255, 0.5)"
                            )
                        , HE.onClick cancelSelection
                        ]

                    else
                        [ HA.style "background-color" "rgb(0, 100, 255)"
                        , HE.onClick (endSelection index)
                        ]

                PromptSelection ->
                    [ HA.style "background-color" "rgb(0, 100, 255)"
                    , HE.onClick (startSelection index)
                    ]

                DontPromptSelection ->
                    [ HA.style "background-color" "black"
                    , HE.onClick (startSelection index)
                    ]
            , [ HA.style "background-color" "black" ]
            )

        dropShadow =
            HA.style "filter"
                ("drop-shadow("
                    ++ pxFloat (toFloat config.dotPixelSize / 4)
                    ++ " "
                    ++ pxFloat (toFloat config.dotPixelSize / 4)
                    ++ " "
                    ++ pxFloat (toFloat config.dotPixelSize / 10)
                    ++ " rgba(0, 0, 0, 0.5))"
                )

        rightStyles =
            [ HA.style "right" "0"
            , HA.style "transform" "translate(50%, -50%)"
            , HA.style "cursor" "pointer"
            , dropShadow
            , HA.style "transition" "background-color 0.2s ease-in"
            ]
                ++ rightModeStyles

        leftStyles =
            [ HA.style "left" "0"
            , HA.style "transform" "translate(-50%, -50%)"
            , dropShadow
            , HA.style "transition" "background-color 0.2s ease-in"
            ]
                ++ leftModeStyles
    in
    Html.div
        ([ HA.style "width" (pxInt config.dotPixelSize)
         , HA.style "height" (pxInt config.dotPixelSize)
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


lines : Float -> Permutation -> Html msg
lines strokeWidth permutation =
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
                (\toIndex ->
                    List.Extra.getAt toIndex permutation
                        |> Maybe.map
                            (\fromIndex ->
                                line
                                    { totalVars = numVars
                                    , fromIndex = fromIndex
                                    , toIndex = toIndex
                                    , strokeWidth = strokeWidth
                                    }
                            )
                )
                (List.range 0 (numVars - 1))
            )
        ]


line : { totalVars : Int, fromIndex : Int, toIndex : Int, strokeWidth : Float } -> Svg msg
line { totalVars, fromIndex, toIndex, strokeWidth } =
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
        , Svg.Attributes.strokeWidth (String.fromFloat strokeWidth)
        ]
        []


encode : Permutation -> Encode.Value
encode adjustments =
    Encode.list Encode.int adjustments


toUrlString : Permutation -> String
toUrlString permutation =
    permutation
        |> List.map String.fromInt
        |> String.join ","


fromUrlString : String -> Maybe Permutation
fromUrlString str =
    str
        |> String.split ","
        |> List.map String.toInt
        |> Maybe.Extra.combine


random : Int -> Random.Generator Permutation
random numVars =
    -- Version that produces a one to one permutation
    Random.List.shuffle (List.range 0 (numVars - 1))
