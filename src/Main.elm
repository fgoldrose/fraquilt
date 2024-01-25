module Main exposing (..)

import Array
import Browser
import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Random
import Settings exposing (Settings)


type alias Model =
    { settings : Settings
    , randomSeed : Random.Seed
    }


type Msg
    = NoOp
    | Randomize
    | UpdateInitialVar Int String
    | ChangeLevel String


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
                            { numVars = 4, level = settings.level }
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


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "height" "100vh"
        , HA.style "gap" "16px"
        ]
        [ Settings.viewEditSettings
            { settings =
                model.settings
            , updateInitialVar = UpdateInitialVar
            , changeLevel = ChangeLevel
            }
        , Html.div
            [ HA.style "width" "512px"
            , HA.style "height" "512px"
            , HA.style "cursor" "pointer"
            , HE.onClick Randomize
            ]
            [ Html.canvas
                [ HA.id "canvas"
                , HA.style "image-rendering" "pixelated"
                , HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "border" "2px solid black"
                ]
                []
            ]
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
                (Settings.random
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
