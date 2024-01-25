module ColorAdjustments exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Messages exposing (Msg(..))
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Types exposing (Quadrant(..))


type ColorAdjustments
    = ColorAdjustments (Array Int)


getNumVars : ColorAdjustments -> Int
getNumVars (ColorAdjustments colorArray) =
    Array.length colorArray


setNewLine : Int -> Int -> ColorAdjustments -> ColorAdjustments
setNewLine fromIndex toIndex (ColorAdjustments colorArray) =
    let
        newColorArray =
            Array.set fromIndex toIndex colorArray
    in
    ColorAdjustments newColorArray


view : ColorAdjustments -> Quadrant -> Maybe Int -> Html Msg
view colorAdjustments quadrant maybeSelectedIndex =
    let
        numVars =
            getNumVars colorAdjustments

        listRange =
            List.range 0 (numVars - 1)

        pixelSize =
            ((20 * numVars) |> String.fromInt) ++ "px"
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
            (lines colorAdjustments
                :: List.map
                    (dot
                        { side = Left
                        , totalVars = numVars
                        , quadrant = quadrant
                        , maybeSelectedIndex = maybeSelectedIndex
                        }
                    )
                    listRange
                ++ List.map
                    (dot
                        { side = Right
                        , totalVars = numVars
                        , quadrant = quadrant
                        , maybeSelectedIndex = maybeSelectedIndex
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
    , maybeSelectedIndex : Maybe Int
    }
    -> Int
    -> Html Msg
dot { side, totalVars, quadrant, maybeSelectedIndex } index =
    Html.div
        ([ HA.style "background-color" "black"
         , HA.style "width" "20px"
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
                        [ HA.style "left" "0"
                        , HA.style "transform" "translate(-50%, -50%)"
                        , HA.style "cursor" "pointer"
                        ]
                            ++ (case maybeSelectedIndex of
                                    Just selectedIndex ->
                                        if selectedIndex == index then
                                            [ HA.style "background-color" "rgb(0, 100, 255)"
                                            , HA.style "outline" "2px solid rgba(0, 100, 255, 0.5)"
                                            ]

                                        else
                                            [ HE.onClick (StartSelection quadrant index) ]

                                    Nothing ->
                                        [ HE.onClick (StartSelection quadrant index) ]
                               )

                    Right ->
                        [ HA.style "right" "0"
                        , HA.style "transform" "translate(50%, -50%)"
                        ]
                            ++ (case maybeSelectedIndex of
                                    Just _ ->
                                        [ HA.style "cursor" "pointer"
                                        , HE.onClick (EndSelection index)
                                        ]

                                    Nothing ->
                                        []
                               )
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
        , HA.style "inset" "0"
        ]
        [ Svg.svg
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , HA.style "overflow" "visible"
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
