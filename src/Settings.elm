port module Settings exposing (..)

import Array exposing (Array)
import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Messages exposing (Msg(..))
import Random
import Types exposing (Mode(..), Quadrant(..), SelectionState(..))


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : Array Int
    , tl : ColorAdjustments
    , tr : ColorAdjustments
    , bl : ColorAdjustments
    , br : ColorAdjustments
    , selectionState : SelectionState
    }


render : Settings -> Cmd msg
render settings =
    renderImage <|
        Encode.object
            [ ( "level", Encode.int settings.level )
            , ( "initialVariables", Encode.array Encode.int settings.initialVariables )
            , ( "colorAdjustments"
              , Encode.object
                    [ ( "tl", ColorAdjustments.encode settings.tl )
                    , ( "tr", ColorAdjustments.encode settings.tr )
                    , ( "bl", ColorAdjustments.encode settings.bl )
                    , ( "br", ColorAdjustments.encode settings.br )
                    ]
              )
            ]


viewEditSettings : Mode -> Settings -> Html Msg
viewEditSettings currentMode settings =
    let
        modeToggleButton : Mode -> Html Msg
        modeToggleButton modeButton =
            Html.button
                [ HA.style "border" "none"
                , HA.style "outline" "none"
                , HA.style "cursor" "pointer"
                , if modeButton == currentMode then
                    HA.style "background-color" "rgb(200, 200, 200)"

                  else
                    HA.class ""
                , HE.onClick (ToggleMode modeButton)
                ]
                [ Html.text
                    (case modeButton of
                        Permutation ->
                            "Permutation"

                        Free ->
                            "Free"
                    )
                ]
    in
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
            , Html.div
                [ HA.style "display" "flex"
                , HA.style "outline" "1px solid black"
                , HA.style "border-radius" "5px"
                ]
                [ modeToggleButton Permutation, modeToggleButton Free ]
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
            , viewColorAdjustmentGrid currentMode settings
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


viewColorAdjustmentGrid : Mode -> Settings -> Html Msg
viewColorAdjustmentGrid mode settings =
    [ Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "100px 100px"
        , HA.style "grid-gap" "30px"
        ]
        [ ColorAdjustments.view settings.tl
            TopLeft
            mode
            settings.selectionState
        , ColorAdjustments.view settings.tr
            TopRight
            mode
            settings.selectionState
        , ColorAdjustments.view settings.bl
            BottomLeft
            mode
            settings.selectionState
        , ColorAdjustments.view settings.br
            BottomRight
            mode
            settings.selectionState
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
            , tl = tl
            , tr = tr
            , bl = bl
            , br = br
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
            , tl = tl
            , tr = tr
            , bl = bl
            , br = br
            , selectionState = NoneSelected
            }
        )
        (Random.map Array.fromList (Random.list numVars (Random.int 0 255)))
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
