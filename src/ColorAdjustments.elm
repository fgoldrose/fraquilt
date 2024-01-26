module ColorAdjustments exposing (..)

import Array exposing (Array)
import DnDList
import DragAndDrop
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
import Types exposing (Quadrant(..))


type alias ColorAdjustments =
    List Int


getNumVars : ColorAdjustments -> Int
getNumVars colorArray =
    List.length colorArray


setNewLine : Int -> Int -> ColorAdjustments -> ColorAdjustments
setNewLine fromIndex toIndex colorArray =
    List.Extra.setAt fromIndex toIndex colorArray


view : { colorAdjustments : ColorAdjustments, dnd : DnDList.Model } -> Quadrant -> Maybe Int -> Html Msg
view { colorAdjustments, dnd } quadrant maybeSelectedIndex =
    let
        numVars =
            getNumVars colorAdjustments

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
            (lines colorAdjustments dnd (DragAndDrop.getSystemForQuadrant quadrant)
                :: List.map
                    (dot
                        { side = Left
                        , totalVars = numVars
                        , quadrant = quadrant
                        , maybeSelectedIndex = maybeSelectedIndex
                        , dndModel = dnd
                        }
                    )
                    listRange
                ++ List.map
                    (dot
                        { side = Right
                        , totalVars = numVars
                        , quadrant = quadrant
                        , maybeSelectedIndex = maybeSelectedIndex
                        , dndModel = dnd
                        }
                    )
                    listRange
            )
        , ghostView dnd (DragAndDrop.getSystemForQuadrant quadrant)
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
    , dndModel : DnDList.Model
    }
    -> Int
    -> Html Msg
dot { side, totalVars, quadrant, maybeSelectedIndex, dndModel } index =
    let
        dndSystem =
            DragAndDrop.getSystemForQuadrant quadrant

        dotId =
            "dot-"
                ++ String.fromInt index
                ++ (case quadrant of
                        TopLeft ->
                            "-top-left"

                        TopRight ->
                            "-top-right"

                        BottomLeft ->
                            "-bottom-left"

                        BottomRight ->
                            "-bottom-right"
                   )

        dndStyles =
            case dndSystem.info dndModel of
                Just { dragIndex, dropIndex } ->
                    if index /= dragIndex && index /= dropIndex then
                        dndSystem.dropEvents index dotId

                    else if index /= dragIndex && index == dropIndex then
                        HA.style "background-color" "rgb(0, 0, 255)"
                            :: HA.style "width" "25px"
                            :: HA.style "height" "25px"
                            :: dndSystem.dropEvents index dotId

                    else
                        [ HA.style "background-color" "rgb(100, 100, 100)"
                        ]

                Nothing ->
                    dndSystem.dragEvents index dotId

        leftStyles =
            [ HA.style "left" "0"
            , HA.style "transform" "translate(-50%, -50%)"
            , HA.style "cursor" "pointer"
            , HA.style "filter" "drop-shadow(5px 5px 2px rgba(0, 0, 0, 0.5))"
            , HA.style "transition" "background-color 0.2s ease-in"
            ]
                ++ (case maybeSelectedIndex of
                        Just selectedIndex ->
                            if selectedIndex == index then
                                [ HA.style "background-color" "rgb(0, 0, 255)"
                                , HA.style "outline" "2px solid rgba(0, 100, 255, 0.5)"
                                ]

                            else
                                [ HA.style "background-color" "rgb(0, 100, 255)"
                                , HE.onClick (StartSelection quadrant index)
                                ]

                        Nothing ->
                            [ HA.style "background-color" "rgb(0, 100, 255)"
                            , HE.onClick (StartSelection quadrant index)
                            ]
                   )
                ++ dndStyles

        rightStyles =
            [ HA.style "right" "0"
            , HA.style "transform" "translate(50%, -50%)"
            , HA.style "filter" "drop-shadow(5px 5px 2px rgba(0, 0, 0, 0.5))"
            , HA.style "transition" "background-color 0.2s ease-in"
            ]
                ++ (case maybeSelectedIndex of
                        Just _ ->
                            [ HA.style "cursor" "pointer"
                            , HE.onClick (EndSelection index)
                            , HA.style "background-color" "rgb(0, 100, 255)"
                            ]

                        Nothing ->
                            [ HA.style "background-color" "black" ]
                   )
    in
    Html.div
        ([ HA.style "width" "20px"
         , HA.style "height" "20px"
         , HA.style "border-radius" "100%"
         , HA.style "position" "absolute"
         , HA.id dotId
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


lines : ColorAdjustments -> DnDList.Model -> DnDList.System Int Msg -> Html Msg
lines colorAdjustments dnd system =
    let
        numVars =
            List.length colorAdjustments

        ( ghostLine, skipIndex ) =
            case system.info dnd of
                Just { dragIndex, currentPosition, startPosition } ->
                    ( List.Extra.getAt dragIndex colorAdjustments
                        |> Maybe.map
                            (\toIndex ->
                                [ Svg.line
                                    [ Svg.Attributes.y1
                                        (String.fromFloat
                                            ((currentPosition.y - startPosition.y)
                                                + ((toFloat dragIndex / toFloat (numVars - 1)) * 120)
                                            )
                                        )
                                    , Svg.Attributes.y2 (getTopPositionForIndex { index = toIndex, totalVars = numVars })
                                    , Svg.Attributes.x1 "0%"
                                    , Svg.Attributes.x2 "100%"
                                    , Svg.Attributes.stroke "black"
                                    , Svg.Attributes.strokeWidth "2"
                                    ]
                                    []
                                ]
                            )
                        |> Maybe.withDefault []
                    , Just dragIndex
                    )

                Nothing ->
                    ( [], Nothing )
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
                    if skipIndex == Just fromIndex then
                        Nothing

                    else
                        List.Extra.getAt fromIndex colorAdjustments
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
                ++ ghostLine
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


ghostView : DnDList.Model -> DnDList.System Int Msg -> Html.Html Msg
ghostView dnd system =
    case system.info dnd of
        Just _ ->
            Html.div
                ([ HA.style "width" "20px"
                 , HA.style "height" "20px"
                 , HA.style "border-radius" "100%"
                 , HA.style "background-color" "rgb(0, 100, 255)"
                 , HA.style "position" "absolute"
                 , HA.style "left" "0"
                 ]
                    ++ system.ghostStyles dnd
                )
                []

        _ ->
            Html.text ""


encode : ColorAdjustments -> Encode.Value
encode adjustments =
    Encode.list Encode.int adjustments


random : Int -> Random.Generator ColorAdjustments
random numVars =
    Random.list numVars (Random.int 0 (numVars - 1))


randomPermutation : Int -> Random.Generator ColorAdjustments
randomPermutation numVars =
    -- Version that produces a one to one permutation
    Random.List.shuffle (List.range 0 (numVars - 1))
