port module Settings exposing (..)

import Array exposing (Array)
import ColorAdjustments exposing (ColorAdjustments)
import DnDList
import DragAndDrop
import Html exposing (Html, label)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Messages exposing (Msg(..))
import Random
import Types exposing (Quadrant(..), SelectionState(..), getSelectedForQuadrant)


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : Array Int
    , tl : ColorAdjustmentsState
    , tr : ColorAdjustmentsState
    , bl : ColorAdjustmentsState
    , br : ColorAdjustmentsState
    , selectionState : SelectionState
    }


type alias ColorAdjustmentsState =
    { colorAdjustments : ColorAdjustments, dnd : DnDList.Model }


render : Settings -> Cmd msg
render settings =
    renderImage <|
        Encode.object
            [ ( "level", Encode.int settings.level )
            , ( "initialVariables", Encode.array Encode.int settings.initialVariables )
            , ( "colorAdjustments"
              , Encode.object
                    [ ( "tl", ColorAdjustments.encode settings.tl.colorAdjustments )
                    , ( "tr", ColorAdjustments.encode settings.tr.colorAdjustments )
                    , ( "bl", ColorAdjustments.encode settings.bl.colorAdjustments )
                    , ( "br", ColorAdjustments.encode settings.br.colorAdjustments )
                    ]
              )
            ]


viewEditSettings : Settings -> Html Msg
viewEditSettings settings =
    Html.div
        [ HA.style "height" "100%"
        , HA.style "max-height" "100vh"
        , HA.style "flex-grow" "2"
        , HA.style "overflow-y" "scroll"
        , HA.style "background-color" "rgb(240, 240, 240)"
        , HA.style "position" "relative"
        ]
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "10px"
            , HA.style "left" "10px"
            , HA.style "display" "flex"
            , HA.style "gap" "10px"
            ]
            [ Html.button [ HE.onClick Randomize ] [ Html.text "Random" ]
            , Html.button
                [ HE.onClick RandomizePermutation
                ]
                [ Html.text "Random Permutation" ]
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "gap" "20px"
            , HA.style "margin" "50px 10px"
            ]
            [ Html.div
                [ HA.style "font-size" "20px"
                , HA.style "font-weight" "bold"
                ]
                [ Html.text "Configuration" ]
            , viewLevel settings.level
            , viewColorAdjustmentGrid settings
            , viewNumberOfVariables (Array.length settings.initialVariables)
            , viewInitialVars settings.initialVariables
            ]
        ]


sectionWithName : String -> List (Html msg) -> Html msg
sectionWithName labelText elements =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "5px"
        ]
        (Html.span
            [ HA.style "font-weight" "bold" ]
            [ Html.text labelText ]
            :: elements
        )


sliderWithLabel :
    { id : String
    , value : Int
    , msg : String -> Msg
    , min : Int
    , max : Int
    }
    -> Html Msg
sliderWithLabel { id, value, msg, min, max } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "gap" "5px"
        ]
        [ Html.label
            [ HA.for id
            , HA.style "width" "30px"
            , HA.style "text-align" "right"
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


viewLevel : Int -> Html Msg
viewLevel level =
    [ sliderWithLabel
        { id = "level"
        , value = level
        , msg = ChangeLevel
        , min = 1
        , max = 10
        }
    ]
        |> sectionWithName "Level"


viewNumberOfVariables : Int -> Html Msg
viewNumberOfVariables numVars =
    [ Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "gap" "10px"
        ]
        [ Html.button [ HE.onClick (ChangeNumberOfVariables (numVars - 1)) ]
            [ Html.text "-" ]
        , Html.span [] [ Html.text (String.fromInt numVars) ]
        , Html.button [ HE.onClick (ChangeNumberOfVariables (numVars + 1)) ]
            [ Html.text "+" ]
        ]
    ]
        |> sectionWithName "Number of variables"


viewColorAdjustmentGrid : Settings -> Html Msg
viewColorAdjustmentGrid settings =
    [ Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "100px 100px"
        , HA.style "grid-gap" "30px"
        ]
        [ ColorAdjustments.view settings.tl
            TopLeft
            (getSelectedForQuadrant TopLeft settings.selectionState)
        , ColorAdjustments.view settings.tr
            TopRight
            (getSelectedForQuadrant TopRight settings.selectionState)
        , ColorAdjustments.view settings.bl
            BottomLeft
            (getSelectedForQuadrant BottomLeft settings.selectionState)
        , ColorAdjustments.view settings.br
            BottomRight
            (getSelectedForQuadrant BottomRight settings.selectionState)
        ]
    ]
        |> sectionWithName "Variable swaps"


viewInitialVars : Array Int -> Html Msg
viewInitialVars initialVars =
    List.indexedMap
        (\index initVar ->
            sliderWithLabel
                { id = "initVar-" ++ String.fromInt index
                , value = initVar
                , msg = UpdateInitialVar index
                , min = 0
                , max = 255
                }
        )
        (Array.toList initialVars)
        |> sectionWithName "Initial variables"


random : { numVars : Int, level : Int } -> Random.Generator Settings
random { numVars, level } =
    Random.map5
        (\initialVariables tl tr bl br ->
            { level = level
            , initialVariables = initialVariables
            , tl = { colorAdjustments = tl, dnd = DragAndDrop.tlSystem.model }
            , tr = { colorAdjustments = tr, dnd = DragAndDrop.trSystem.model }
            , bl = { colorAdjustments = bl, dnd = DragAndDrop.blSystem.model }
            , br = { colorAdjustments = br, dnd = DragAndDrop.brSystem.model }
            , selectionState = NoneSelected
            }
        )
        (Random.map Array.fromList (Random.list numVars (Random.int 0 255)))
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)


randomPermutations : { numVars : Int, level : Int } -> Random.Generator Settings
randomPermutations { numVars, level } =
    Random.map5
        (\initialVariables tl tr bl br ->
            { level = level
            , initialVariables = initialVariables
            , tl = { colorAdjustments = tl, dnd = DragAndDrop.tlSystem.model }
            , tr = { colorAdjustments = tr, dnd = DragAndDrop.trSystem.model }
            , bl = { colorAdjustments = bl, dnd = DragAndDrop.blSystem.model }
            , br = { colorAdjustments = br, dnd = DragAndDrop.brSystem.model }
            , selectionState = NoneSelected
            }
        )
        (Random.map Array.fromList (Random.list numVars (Random.int 0 255)))
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
