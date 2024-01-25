port module Settings exposing (..)

import Array exposing (Array)
import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
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


viewEditSettings : Settings -> Html Msg
viewEditSettings settings =
    Html.div
        [ HA.style "height" "100%"
        , HA.style "max-height" "100vh"
        , HA.style "flex-grow" "2"
        , HA.style "overflow-y" "scroll"
        , HA.style "background-color" "rgb(240, 240, 240)"
        ]
        [ Html.div
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
            , viewNumberOfVariables (Array.length settings.initialVariables)
            , viewColorAdjustmentGrid settings
            , viewInitialVars settings.initialVariables
            ]
        ]


viewLevel : Int -> Html Msg
viewLevel level =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "10px"
        ]
        [ Html.span [] [ Html.text "Level" ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "row"
            , HA.style "align-items" "center"
            , HA.style "gap" "5px"
            ]
            [ Html.label
                [ HA.for "level"
                , HA.style "width" "30px"
                , HA.style "text-align" "right"
                ]
                [ Html.text (String.fromInt level) ]
            , Html.input
                [ HA.id "level"
                , HA.type_ "range"
                , HA.min "1"
                , HA.max "10"
                , HA.list "level-markers"
                , HA.value (String.fromInt level)
                , HE.onInput ChangeLevel
                ]
                []
            , Html.datalist [ HA.id "level-markers" ]
                (List.map
                    (\l ->
                        Html.option
                            [ HA.value (String.fromInt l) ]
                            [ Html.text (String.fromInt l) ]
                    )
                    (List.range 1 10)
                )
            ]
        ]


viewNumberOfVariables : Int -> Html Msg
viewNumberOfVariables numVars =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "10px"
        ]
        [ Html.span [] [ Html.text "Number of variables" ]
        , Html.div
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


viewColorAdjustmentGrid : Settings -> Html Msg
viewColorAdjustmentGrid settings =
    Html.div
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


viewInitialVars : Array Int -> Html Msg
viewInitialVars initialVars =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "10px"
        ]
        (Html.span [] [ Html.text "Initial values" ]
            :: List.indexedMap
                (\index initVar ->
                    let
                        id =
                            "initVar-" ++ String.fromInt index
                    in
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
                            [ Html.text (String.fromInt initVar) ]
                        , Html.input
                            [ HA.id id
                            , HA.type_ "range"
                            , HA.min "0"
                            , HA.max "255"
                            , HA.value (String.fromInt initVar)
                            , HE.onInput (UpdateInitialVar index)
                            ]
                            []
                        ]
                )
                (Array.toList initialVars)
        )


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
