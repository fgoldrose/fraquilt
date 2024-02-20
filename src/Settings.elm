port module Settings exposing (..)

import Array exposing (Array)
import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Messages exposing (Msg(..))
import Random
import Svg
import Svg.Attributes as SvgAttr
import Types exposing (Mode(..), Quadrant(..), SelectionState(..))


port renderImage : Encode.Value -> Cmd msg


type alias Color =
    -- hex
    String


type alias InitialVariables =
    Array Color


type alias Settings =
    { level : Int
    , initialVariables : InitialVariables
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
            , ( "initialVariables"
              , Encode.array
                    Encode.string
                    settings.initialVariables
              )
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
            ]
            [ Html.button [ HE.onClick Randomize ] [ Html.text "Random" ]
            ]
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "10px"
            , HA.style "right" "10px"
            ]
            [ helpIcon
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


helpIcon : Html Msg
helpIcon =
    Svg.svg
        [ SvgAttr.width "30px"
        , SvgAttr.height "30px"
        , SvgAttr.viewBox "0 0 15 15"
        , SvgAttr.fill "none"
        , HE.onClick (ShowHelpInfo True)
        , HA.style "cursor" "pointer"
        ]
        [ Svg.path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.clipRule "evenodd"
            , SvgAttr.d "M0.877075 7.49972C0.877075 3.84204 3.84222 0.876892 7.49991 0.876892C11.1576 0.876892 14.1227 3.84204 14.1227 7.49972C14.1227 11.1574 11.1576 14.1226 7.49991 14.1226C3.84222 14.1226 0.877075 11.1574 0.877075 7.49972ZM7.49991 1.82689C4.36689 1.82689 1.82708 4.36671 1.82708 7.49972C1.82708 10.6327 4.36689 13.1726 7.49991 13.1726C10.6329 13.1726 13.1727 10.6327 13.1727 7.49972C13.1727 4.36671 10.6329 1.82689 7.49991 1.82689ZM8.24993 10.5C8.24993 10.9142 7.91414 11.25 7.49993 11.25C7.08571 11.25 6.74993 10.9142 6.74993 10.5C6.74993 10.0858 7.08571 9.75 7.49993 9.75C7.91414 9.75 8.24993 10.0858 8.24993 10.5ZM6.05003 6.25C6.05003 5.57211 6.63511 4.925 7.50003 4.925C8.36496 4.925 8.95003 5.57211 8.95003 6.25C8.95003 6.74118 8.68002 6.99212 8.21447 7.27494C8.16251 7.30651 8.10258 7.34131 8.03847 7.37854L8.03841 7.37858C7.85521 7.48497 7.63788 7.61119 7.47449 7.73849C7.23214 7.92732 6.95003 8.23198 6.95003 8.7C6.95004 9.00376 7.19628 9.25 7.50004 9.25C7.8024 9.25 8.04778 9.00601 8.05002 8.70417L8.05056 8.7033C8.05924 8.6896 8.08493 8.65735 8.15058 8.6062C8.25207 8.52712 8.36508 8.46163 8.51567 8.37436L8.51571 8.37433C8.59422 8.32883 8.68296 8.27741 8.78559 8.21506C9.32004 7.89038 10.05 7.35382 10.05 6.25C10.05 4.92789 8.93511 3.825 7.50003 3.825C6.06496 3.825 4.95003 4.92789 4.95003 6.25C4.95003 6.55376 5.19628 6.8 5.50003 6.8C5.80379 6.8 6.05003 6.55376 6.05003 6.25Z"
            , SvgAttr.fill "#000000"
            ]
            []
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
        |> sectionWithName "Number of colors"


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
        |> sectionWithName "Permutations"


viewInitialVars : InitialVariables -> Html Msg
viewInitialVars initialVars =
    List.indexedMap
        (\index initVar ->
            Html.input
                [ HA.type_ "color"
                , HA.id ("initVar-" ++ String.fromInt index)
                , HA.value initVar
                , HE.onInput (UpdateInitialVar index)
                ]
                []
        )
        (Array.toList initialVars)
        |> sectionWithName "Initial Colors"


random : { initVars : InitialVariables, numVars : Int, level : Int } -> Random.Generator Settings
random { initVars, numVars, level } =
    Random.map4
        (\tl tr bl br ->
            { level = level
            , initialVariables = initVars
            , tl = tl
            , tr = tr
            , bl = bl
            , br = br
            , selectionState = NoneSelected
            }
        )
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)


randomPermutations : { initVars : InitialVariables, numVars : Int, level : Int } -> Random.Generator Settings
randomPermutations { initVars, numVars, level } =
    Random.map4
        (\tl tr bl br ->
            { level = level
            , initialVariables = initVars
            , tl = tl
            , tr = tr
            , bl = bl
            , br = br
            , selectionState = NoneSelected
            }
        )
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)
        (ColorAdjustments.randomPermutation numVars)