module Main exposing (..)

import Array
import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Info
import List.Extra
import Messages exposing (Msg(..))
import Permutation
import Random
import Settings exposing (Settings)
import Types exposing (Mode(..), Quadrant(..), SelectionState(..))


type alias Model =
    { settings : Settings
    , randomSeed : Random.Seed
    , mode : Mode
    , showHelp : Bool
    }


randomizeModel : Model -> ( Model, Cmd msg )
randomizeModel model =
    let
        randomizeFunction =
            case model.mode of
                Permutation ->
                    Settings.randomPermutations

                Free ->
                    Settings.random

        ( randomSettings, newSeed ) =
            Random.step
                (randomizeFunction
                    { initVars = model.settings.initialVariables
                    , numVars = Array.length model.settings.initialVariables
                    , level = model.settings.level
                    }
                )
                model.randomSeed
    in
    ( { model
        | settings = randomSettings
        , randomSeed = newSeed
      }
    , Settings.render randomSettings
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Randomize ->
            randomizeModel model

        ClearPermutations ->
            let
                numVars =
                    Array.length settings.initialVariables

                newSettings =
                    { settings
                        | tl = List.range 0 (numVars - 1)
                        , tr = List.range 0 (numVars - 1)
                        , bl = List.range 0 (numVars - 1)
                        , br = List.range 0 (numVars - 1)
                    }
            in
            ( { model
                | settings = newSettings
              }
            , Settings.render newSettings
            )

        UpdateInitialVar index stringValue ->
            let
                newSettings =
                    { settings
                        | initialVariables =
                            Array.set index stringValue settings.initialVariables
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
            if numVars < 2 then
                ( model, Cmd.none )

            else
                let
                    initialVariables =
                        model.settings.initialVariables

                    oldNumVars =
                        Array.length initialVariables

                    newSettings =
                        if numVars == oldNumVars then
                            settings

                        else if numVars > oldNumVars then
                            let
                                varsToAdd =
                                    numVars - oldNumVars
                            in
                            { settings
                                | initialVariables =
                                    Array.append initialVariables
                                        (Array.repeat varsToAdd "#000000")
                                , tl = Permutation.addN varsToAdd settings.tl
                                , tr = Permutation.addN varsToAdd settings.tr
                                , bl = Permutation.addN varsToAdd settings.bl
                                , br = Permutation.addN varsToAdd settings.br
                            }

                        else
                            let
                                varsToRemove =
                                    oldNumVars - numVars
                            in
                            { settings
                                | initialVariables =
                                    Array.slice 0 numVars initialVariables
                                , tl = Permutation.removeN varsToRemove settings.tl
                                , tr = Permutation.removeN varsToRemove settings.tr
                                , bl = Permutation.removeN varsToRemove settings.bl
                                , br = Permutation.removeN varsToRemove settings.br
                            }
                in
                ( { model | settings = newSettings }, Settings.render newSettings )

        CancelSelection ->
            ( { model
                | settings =
                    { settings
                        | selectionState = NoneSelected
                    }
              }
            , Cmd.none
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
                modeFunction =
                    case model.mode of
                        Permutation ->
                            \startIndex adjustments ->
                                List.Extra.swapAt startIndex endIndex adjustments

                        Free ->
                            \startIndex adjustments -> adjustments |> Permutation.setNewLine startIndex endIndex

                newSettings =
                    case settings.selectionState of
                        TLSelected startIndex ->
                            { settings
                                | tl = modeFunction startIndex settings.tl
                            }

                        TRSelected startIndex ->
                            { settings
                                | tr = modeFunction startIndex settings.tr
                            }

                        BLSelected startIndex ->
                            { settings
                                | bl = modeFunction startIndex settings.bl
                            }

                        BRSelected startIndex ->
                            { settings
                                | br = modeFunction startIndex settings.br
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

        ToggleMode mode ->
            case mode of
                Permutation ->
                    { model | mode = Permutation }
                        |> randomizeModel

                Free ->
                    ( { model | mode = Free }, Cmd.none )

        ShowHelpInfo bool ->
            ( { model | showHelp = bool }, Cmd.none )


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
                , HA.style "width" "90vmin"
                , HA.style "height" "90vmin"
                ]
                [ Html.canvas
                    [ HA.id "canvas"
                    , HA.style "width" "100%"
                    , HA.style "height" "100%"
                    , HA.style "image-rendering" "pixelated"
                    , HA.style "border" "2px solid black"
                    , HE.onClick Randomize
                    ]
                    []
                ]
            ]
        , if model.showHelp then
            Info.helpView

          else
            Settings.viewEditSettings model.mode model.settings
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
                    { initVars = Array.fromList [ "#ffffff", "#808080", "#000000" ]
                    , numVars = 3
                    , level = 9
                    }
                )
                randomSeed
    in
    ( { settings = randomSettings
      , randomSeed = newSeed
      , mode = Permutation
      , showHelp = False
      }
    , Settings.render randomSettings
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
