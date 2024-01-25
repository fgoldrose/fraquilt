module Main exposing (..)

import Array
import Browser
import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Messages exposing (Msg(..))
import Random
import Settings exposing (Settings)
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
                newSettings =
                    case settings.selectionState of
                        TLSelected startIndex ->
                            { settings
                                | tl =
                                    settings.tl
                                        |> ColorAdjustments.setNewLine
                                            startIndex
                                            endIndex
                            }

                        TRSelected startIndex ->
                            { settings
                                | tr =
                                    settings.tr
                                        |> ColorAdjustments.setNewLine
                                            startIndex
                                            endIndex
                            }

                        BLSelected startIndex ->
                            { settings
                                | bl =
                                    settings.bl
                                        |> ColorAdjustments.setNewLine
                                            startIndex
                                            endIndex
                            }

                        BRSelected startIndex ->
                            { settings
                                | br =
                                    settings.br
                                        |> ColorAdjustments.setNewLine
                                            startIndex
                                            endIndex
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


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
