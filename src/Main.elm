module Main exposing (..)

import Browser
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Randomize ->
            let
                ( randomSettings, newSeed ) =
                    Random.step
                        (Settings.random
                            { numVars = 4, level = model.settings.level }
                        )
                        model.randomSeed
            in
            ( { settings = randomSettings
              , randomSeed = newSeed
              }
            , Settings.render randomSettings
            )


view : Model -> Html Msg
view model =
    Html.div
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
            ]
            []
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
