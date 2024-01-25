port module Settings exposing (..)

import Array exposing (Array)
import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Random


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : Array Int
    , tl : ColorAdjustments
    , tr : ColorAdjustments
    , bl : ColorAdjustments
    , br : ColorAdjustments
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


viewEditSettings :
    { settings : Settings
    , updateInitialVar : Int -> String -> msg
    , changeLevel : String -> msg
    , changeNumVars : Int -> msg
    }
    -> Html msg
viewEditSettings { settings, updateInitialVar, changeLevel, changeNumVars } =
    let
        numVars =
            Array.length settings.initialVariables

        numberOfVariables =
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
                    [ Html.button [ HE.onClick (changeNumVars (numVars - 1)) ] [ Html.text "-" ]
                    , Html.span [] [ Html.text (String.fromInt numVars) ]
                    , Html.button [ HE.onClick (changeNumVars (numVars + 1)) ] [ Html.text "+" ]
                    ]
                ]

        colorAdjustmentsGrid =
            Html.div
                [ HA.style "display" "grid"
                , HA.style "grid-template-columns" "100px 100px"
                , HA.style "grid-gap" "20px"
                ]
                [ ColorAdjustments.view settings.tl
                , ColorAdjustments.view settings.tr
                , ColorAdjustments.view settings.bl
                , ColorAdjustments.view settings.br
                ]

        initialVars =
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
                                    , HE.onInput (updateInitialVar index)
                                    ]
                                    []
                                ]
                        )
                        (Array.toList settings.initialVariables)
                )

        level =
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
                        [ Html.text (String.fromInt settings.level) ]
                    , Html.input
                        [ HA.id "level"
                        , HA.type_ "range"
                        , HA.min "1"
                        , HA.max "10"
                        , HA.list "level-markers"
                        , HA.value (String.fromInt settings.level)
                        , HE.onInput changeLevel
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
    in
    Html.div
        [ HA.style "height" "100%"
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
            , numberOfVariables
            , colorAdjustmentsGrid
            , Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "row"
                , HA.style "justify-content" "space-between"
                ]
                [ initialVars, level ]
            ]
        ]


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
            }
        )
        (Random.map Array.fromList (Random.list numVars (Random.int 0 255)))
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
