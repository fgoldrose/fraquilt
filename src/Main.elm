module Main exposing (..)

import Array
import Browser
import ColorAdjustments exposing (ColorAdjustments)
import DnDList
import DragAndDrop
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Messages exposing (Msg(..))
import Random
import Settings exposing (Settings)
import Svg.Attributes exposing (color)
import Types exposing (Quadrant(..), SelectionState(..))


type alias Model =
    { settings : Settings
    , randomSeed : Random.Seed
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings, randomSeed } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Randomize ->
            let
                ( randomSettings, newSeed ) =
                    Random.step
                        (Settings.random
                            { numVars = Array.length settings.initialVariables
                            , level = settings.level
                            }
                        )
                        randomSeed
            in
            ( { settings = randomSettings
              , randomSeed = newSeed
              }
            , Settings.render randomSettings
            )

        RandomizePermutation ->
            let
                ( randomSettings, newSeed ) =
                    Random.step
                        (Settings.randomPermutations
                            { numVars = Array.length settings.initialVariables
                            , level = settings.level
                            }
                        )
                        randomSeed
            in
            ( { settings = randomSettings
              , randomSeed = newSeed
              }
            , Settings.render randomSettings
            )

        UpdateInitialVar index stringValue ->
            case String.toInt stringValue of
                Nothing ->
                    ( model, Cmd.none )

                Just value ->
                    let
                        newSettings =
                            { settings
                                | initialVariables =
                                    Array.set index value settings.initialVariables
                            }
                    in
                    ( { model | settings = newSettings }
                    , Settings.render newSettings
                    )

        ChangeLevel levelString ->
            case String.toInt levelString of
                Nothing ->
                    ( model, Cmd.none )

                Just value ->
                    let
                        newSettings =
                            { settings
                                | level = value
                            }
                    in
                    ( { model | settings = newSettings }
                    , Settings.render newSettings
                    )

        ChangeNumberOfVariables numVars ->
            let
                ( randomSettings, newSeed ) =
                    Random.step
                        (Settings.randomPermutations
                            { numVars = numVars, level = settings.level }
                        )
                        randomSeed
            in
            ( { settings = randomSettings
              , randomSeed = newSeed
              }
            , Settings.render randomSettings
            )

        StartSelection quadrant index ->
            ( { model
                | settings =
                    { settings
                        | selectionState =
                            case quadrant of
                                TopLeft ->
                                    TLSelected index

                                TopRight ->
                                    TRSelected index

                                BottomLeft ->
                                    BLSelected index

                                BottomRight ->
                                    BRSelected index
                    }
              }
            , Cmd.none
            )

        EndSelection endIndex ->
            let
                tl =
                    settings.tl

                tr =
                    settings.tr

                bl =
                    settings.bl

                br =
                    settings.br

                newSettings =
                    case settings.selectionState of
                        TLSelected startIndex ->
                            { settings
                                | tl =
                                    { tl
                                        | colorAdjustments =
                                            tl.colorAdjustments
                                                |> ColorAdjustments.setNewLine
                                                    startIndex
                                                    endIndex
                                    }
                            }

                        TRSelected startIndex ->
                            { settings
                                | tr =
                                    { tr
                                        | colorAdjustments =
                                            tr.colorAdjustments
                                                |> ColorAdjustments.setNewLine
                                                    startIndex
                                                    endIndex
                                    }
                            }

                        BLSelected startIndex ->
                            { settings
                                | bl =
                                    { bl
                                        | colorAdjustments =
                                            bl.colorAdjustments
                                                |> ColorAdjustments.setNewLine
                                                    startIndex
                                                    endIndex
                                    }
                            }

                        BRSelected startIndex ->
                            { settings
                                | br =
                                    { br
                                        | colorAdjustments =
                                            br.colorAdjustments
                                                |> ColorAdjustments.setNewLine
                                                    startIndex
                                                    endIndex
                                    }
                            }

                        NoneSelected ->
                            settings
            in
            ( { model
                | settings =
                    { newSettings | selectionState = NoneSelected }
              }
            , Settings.render newSettings
            )

        DnDMsg quadrant dndMsg ->
            let
                rerender =
                    if settings /= newSettings then
                        Settings.render newSettings

                    else
                        Cmd.none

                ( newSettings, commands ) =
                    case quadrant of
                        TopLeft ->
                            let
                                ( tlDnd, tlColorAdjustments ) =
                                    DragAndDrop.tlSystem.update dndMsg settings.tl.dnd settings.tl.colorAdjustments
                            in
                            ( { settings
                                | tl = { colorAdjustments = tlColorAdjustments, dnd = tlDnd }
                              }
                            , DragAndDrop.tlSystem.commands tlDnd
                            )

                        TopRight ->
                            let
                                ( trDnd, trColorAdjustments ) =
                                    DragAndDrop.trSystem.update dndMsg settings.tr.dnd settings.tr.colorAdjustments
                            in
                            ( { settings
                                | tr = { colorAdjustments = trColorAdjustments, dnd = trDnd }
                              }
                            , DragAndDrop.trSystem.commands trDnd
                            )

                        BottomLeft ->
                            let
                                ( blDnd, blColorAdjustments ) =
                                    DragAndDrop.blSystem.update dndMsg settings.bl.dnd settings.bl.colorAdjustments
                            in
                            ( { settings
                                | bl = { colorAdjustments = blColorAdjustments, dnd = blDnd }
                              }
                            , DragAndDrop.blSystem.commands blDnd
                            )

                        BottomRight ->
                            let
                                ( brDnd, brColorAdjustments ) =
                                    DragAndDrop.brSystem.update dndMsg settings.br.dnd settings.br.colorAdjustments
                            in
                            ( { settings
                                | br = { colorAdjustments = brColorAdjustments, dnd = brDnd }
                              }
                            , DragAndDrop.brSystem.commands brDnd
                            )
            in
            ( { model | settings = newSettings }
            , Cmd.batch [ commands, rerender ]
            )


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "space-between"
        , HA.style "height" "100vh"
        , HA.style "width" "100vw"
        , HA.style "flex-wrap" "wrap"
        , HA.style "font-family" "sans-serif"
        , HA.style "overflow" "auto"
        ]
        [ Html.div
            [ HA.style "cursor" "pointer"
            , HA.style "margin" "10px"
            , HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "flex-grow" "1"
            ]
            [ Html.div
                [ HA.style "max-width" "90vw"
                , HA.style "max-height" "90vh"
                , HA.style "width" "90vh"
                , HA.style "height" "90vh"
                ]
                [ Html.canvas
                    [ HA.id "canvas"
                    , HA.style "width" "100%"
                    , HA.style "height" "100%"
                    , HA.style "image-rendering" "pixelated"
                    , HA.style "border" "2px solid black"
                    , HE.onClick RandomizePermutation
                    ]
                    []
                ]
            ]
        , Settings.viewEditSettings model.settings
        ]


type alias Flags =
    { randomSeed : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        randomSeed =
            Random.initialSeed flags.randomSeed

        ( randomSettings, newSeed ) =
            Random.step
                (Settings.randomPermutations
                    { numVars = 4, level = 9 }
                )
                randomSeed
    in
    ( { settings = randomSettings
      , randomSeed = newSeed
      }
    , Settings.render randomSettings
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ DragAndDrop.tlSystem.subscriptions model.settings.tl.dnd
        , DragAndDrop.trSystem.subscriptions model.settings.tr.dnd
        , DragAndDrop.blSystem.subscriptions model.settings.bl.dnd
        , DragAndDrop.brSystem.subscriptions model.settings.br.dnd
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
