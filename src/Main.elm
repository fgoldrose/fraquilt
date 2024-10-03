module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Colors
import Html
import Html.Attributes as HA
import Html.Events as HE
import Messages exposing (Msg(..))
import Permutation
import PermutationGrid
import Random
import Routing
import Settings exposing (Settings)
import Task
import Tutorial
import Types exposing (Quadrant(..), SelectionState(..))
import Url exposing (Url)


type alias Model =
    { settings : Settings
    , key : Nav.Key
    , randomSeed : Random.Seed
    , selectionState : SelectionState
    , tutorial : Maybe Tutorial.Page
    , window : { width : Int, height : Int }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUrlChange url ->
            case Routing.parseUrl url of
                Routing.Tutorial tutorialRoute ->
                    ( { model | tutorial = Just (Tutorial.initPage tutorialRoute) }
                    , Task.attempt (\_ -> NoOp) (Browser.Dom.setViewportOf "page" 0 0)
                    )

                Routing.App Nothing ->
                    ( { model | tutorial = Nothing }
                    , Settings.change model.key settings
                    )

                Routing.App (Just urlSettings) ->
                    if urlSettings == settings then
                        ( { model | tutorial = Nothing }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | tutorial = Nothing
                            , settings = urlSettings
                          }
                        , Settings.render urlSettings
                        )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        Randomize { symmetric } ->
            let
                randomFunction =
                    if symmetric then
                        PermutationGrid.randomSymmetric

                    else
                        PermutationGrid.random

                ( randomPermutations, newSeed ) =
                    Random.step
                        (randomFunction (Colors.count model.settings.initialVariables))
                        model.randomSeed

                updatedSettings =
                    { settings | permutations = randomPermutations }
            in
            ( { model
                | settings = updatedSettings
                , randomSeed = newSeed
              }
            , Settings.change model.key updatedSettings
            )

        ClearPermutations ->
            let
                numVars =
                    Colors.count settings.initialVariables

                newSettings =
                    { settings
                        | permutations = PermutationGrid.clear numVars
                    }
            in
            ( { model
                | settings = newSettings
              }
            , Settings.change model.key newSettings
            )

        UpdateInitialVar index stringValue ->
            let
                newSettings =
                    { settings
                        | initialVariables =
                            Colors.set index stringValue settings.initialVariables
                    }
            in
            ( { model | settings = newSettings }
            , Settings.change model.key newSettings
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
                    , Settings.change model.key newSettings
                    )

        ChangeNumberOfVariables numVars ->
            if numVars < 2 then
                ( model, Cmd.none )

            else
                let
                    initialVariables =
                        model.settings.initialVariables

                    oldNumVars =
                        Colors.count initialVariables

                    newSettings =
                        if numVars == oldNumVars then
                            settings

                        else if numVars > oldNumVars then
                            let
                                varsToAdd =
                                    numVars - oldNumVars
                            in
                            { settings
                                | initialVariables = Colors.addN varsToAdd initialVariables
                                , permutations =
                                    { tl = Permutation.addN varsToAdd settings.permutations.tl
                                    , tr = Permutation.addN varsToAdd settings.permutations.tr
                                    , bl = Permutation.addN varsToAdd settings.permutations.bl
                                    , br = Permutation.addN varsToAdd settings.permutations.br
                                    }
                            }

                        else
                            let
                                varsToRemove =
                                    oldNumVars - numVars
                            in
                            { settings
                                | initialVariables = Colors.removeN varsToRemove initialVariables
                                , permutations =
                                    { tl = Permutation.removeN varsToRemove settings.permutations.tl
                                    , tr = Permutation.removeN varsToRemove settings.permutations.tr
                                    , bl = Permutation.removeN varsToRemove settings.permutations.bl
                                    , br = Permutation.removeN varsToRemove settings.permutations.br
                                    }
                            }
                in
                ( { model | settings = newSettings }
                , Settings.change model.key newSettings
                )

        CancelSelection ->
            ( { model
                | selectionState = NoneSelected
              }
            , Cmd.none
            )

        StartSelection quadrant index ->
            ( { model
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
            , Cmd.none
            )

        EndSelection endIndex ->
            let
                newSettings =
                    { settings
                        | permutations =
                            PermutationGrid.endSelection
                                model.selectionState
                                endIndex
                                settings.permutations
                    }
            in
            ( { model
                | settings = newSettings
                , selectionState = NoneSelected
              }
            , Settings.change model.key newSettings
            )

        TutorialMsg tutorialMsg ->
            case model.tutorial of
                Just page ->
                    let
                        ( newPage, newCmd ) =
                            Tutorial.update tutorialMsg page
                    in
                    ( { model | tutorial = Just newPage }, Cmd.map TutorialMsg newCmd )

                Nothing ->
                    ( model, Cmd.none )

        WindowChanged width height ->
            ( { model | window = { width = width, height = height } }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Fraquilt"
    , body =
        case model.tutorial of
            Just page ->
                [ Tutorial.view page model.window.height |> Html.map TutorialMsg ]

            Nothing ->
                [ Html.div
                    [ HA.class "flex items-center h-screen w-screen font-sans overflow-auto justify-between flex-wrap tall:flex-nowrap tall:justify-normal tall:flex-col tall:overflow-hidden" ]
                    [ Html.div
                        [ HA.class "cursor-pointer m-3 flex items-center justify-center grow" ]
                        [ Html.div
                            [ HA.class "max-w-[90vw] max-h-[90vh] w-[90vmin] h-[90vmin]"
                            ]
                            [ Html.canvas
                                [ HA.id "canvas"
                                , HA.style "width" "100%"
                                , HA.style "height" "100%"
                                , HA.style "image-rendering" "pixelated"
                                , HA.style "border" "2px solid black"
                                , HE.onClick (Randomize { symmetric = True })
                                ]
                                []
                            ]
                        ]
                    , Settings.viewEditSettings model.selectionState model.settings
                    ]
                ]
    }


type alias Flags =
    { randomSeed : Int
    , window : { width : Int, height : Int }
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        makeModel : Maybe Settings -> Maybe Tutorial.Page -> Model
        makeModel maybeSettings tutorial =
            let
                { settings, randomSeed } =
                    case maybeSettings of
                        Nothing ->
                            let
                                ( permutations, seed ) =
                                    Random.step
                                        (PermutationGrid.randomSymmetric 5)
                                        (Random.initialSeed flags.randomSeed)
                            in
                            { settings =
                                { level = 9
                                , initialVariables = Colors.init5
                                , permutations = permutations
                                }
                            , randomSeed = seed
                            }

                        Just s ->
                            { settings = s
                            , randomSeed = Random.initialSeed flags.randomSeed
                            }
            in
            { settings = settings
            , key = key
            , randomSeed = randomSeed
            , selectionState = NoneSelected
            , tutorial = tutorial
            , window = flags.window
            }

        -- Todo: handle decode error
    in
    case Routing.parseUrl url of
        Routing.Tutorial tutorialRoute ->
            ( makeModel Nothing (Just (Tutorial.initPage tutorialRoute))
            , Cmd.none
            )

        Routing.App maybeSettings ->
            let
                model =
                    makeModel maybeSettings Nothing
            in
            ( model, Settings.change key model.settings )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowChanged


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }
