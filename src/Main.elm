module Main exposing (..)

import Array
import Browser
import ColorAdjustments
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Info
import List.Extra
import Messages exposing (Msg(..))
import Random
import Settings exposing (Settings)
import Types exposing (Mode(..), Quadrant(..), SelectionState(..))


type alias Model =
    { settings : Settings
    , randomSeed : Random.Seed
    , mode : Mode
    , showHelp : Bool
    }


randomizeModel : Int -> Model -> ( Model, Cmd msg )
randomizeModel numVars model =
    let
        oldNumVars =
            Array.length model.settings.initialVariables

        initVars =
            if oldNumVars == numVars then
                model.settings.initialVariables

            else if numVars > oldNumVars then
                Array.append model.settings.initialVariables
                    (Array.repeat (numVars - oldNumVars) "#000000")

            else
                Array.slice 0 numVars model.settings.initialVariables

        randomizeFunction =
            case model.mode of
                Permutation ->
                    Settings.randomPermutations

                Free ->
                    Settings.random

        ( randomSettings, newSeed ) =
            Random.step
                (randomizeFunction
                    { initVars = initVars
                    , numVars = numVars
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
            model |> randomizeModel (Array.length model.settings.initialVariables)

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
            model |> randomizeModel numVars

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
                            \startIndex adjustments -> adjustments |> ColorAdjustments.setNewLine startIndex endIndex

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
                        |> randomizeModel (Array.length settings.initialVariables)

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
                , HA.style "width" "90vh"
                , HA.style "height" "90vh"
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
