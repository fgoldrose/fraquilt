module ColorAdjustments exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Encode as Encode
import Random
import Svg exposing (Svg)
import Svg.Attributes


type ColorAdjustments
    = ColorAdjustments (Array Int)


view : ColorAdjustments -> Html msg
view colorAdjustments =
    let
        numVars =
            getNumVars colorAdjustments

        listRange =
            List.range 0 (numVars - 1)

        leftDots =
            List.map (dot { side = Left, totalVars = numVars }) listRange

        rightDots =
            List.map (dot { side = Right, totalVars = numVars }) listRange
    in
    Html.div
        [ HA.style "background-color" "rgb(200, 200, 200)"
        , HA.style "border-radius" "6%"
        , HA.style "padding" "20px 0"
        ]
        [ Html.div
            [ HA.style "position" "relative"
            , HA.style "width" "100px"
            , HA.style "height" "100px"
            ]
            (lines colorAdjustments :: leftDots ++ rightDots)
        ]


getNumVars : ColorAdjustments -> Int
getNumVars (ColorAdjustments colorArray) =
    Array.length colorArray


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


dot : { side : Side, totalVars : Int } -> Int -> Html msg
dot { side, totalVars } index =
    Html.div
        ([ HA.style "background-color" "black"
         , HA.style "width" "10px"
         , HA.style "height" "10px"
         , HA.style "border-radius" "100%"
         , HA.style "position" "absolute"
         , HA.style "top"
            (getTopPositionForIndex
                { index = index, totalVars = totalVars }
            )
         ]
            ++ (case side of
                    Left ->
                        [ HA.style "left" "0"
                        , HA.style "transform" "translate(-50%, -50%)"
                        ]

                    Right ->
                        [ HA.style "right" "0"
                        , HA.style "transform" "translate(50%, -50%)"
                        ]
               )
        )
        []


lines : ColorAdjustments -> Html msg
lines (ColorAdjustments colorAdjustments) =
    let
        numVars =
            Array.length colorAdjustments
    in
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "inset" "-5px 0px"
        ]
        [ Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.viewBox "0 0 100 100"
            ]
            (List.filterMap
                (\fromIndex ->
                    Array.get fromIndex colorAdjustments
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


encode : ColorAdjustments -> Encode.Value
encode (ColorAdjustments adjustments) =
    Encode.array Encode.int adjustments


random : Int -> Random.Generator ColorAdjustments
random numVars =
    Random.list numVars (Random.int 0 (numVars - 1))
        |> Random.map (Array.fromList >> ColorAdjustments)
