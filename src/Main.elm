module Main exposing (..)

import AppUrl
import Browser
import Browser.Navigation as Nav
import Colors
import Html
import Html.Attributes as HA
import Html.Events as HE
import Info
import Messages exposing (Msg(..))
import Permutation
import PermutationGrid
import Random
import Settings exposing (Settings)
import Tutorial
import Types exposing (Quadrant(..), SelectionState(..))
import Url exposing (Url)


type alias Model =
    { settings : Settings
    , key : Nav.Key
    , randomSeed : Random.Seed
    , selectionState : SelectionState
    , showHelp : Bool
    , tutorial : Maybe Tutorial.Page
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUrlChange url ->
            let
                appUrl =
                    AppUrl.fromUrl url
            in
            case appUrl.path of
                [ "fraquilt", "tutorial" ] ->
                    ( { model | tutorial = Just Tutorial.Intro }
                    , Cmd.none
                    )

                [ "fraquilt", "tutorial", pageNumber ] ->
                    ( { model | tutorial = Just (Tutorial.initPage pageNumber) }
                    , Cmd.none
                    )

                _ ->
                    case Settings.fromUrl (AppUrl.fromUrl url) of
                        Nothing ->
                            ( { model | tutorial = Nothing }
                            , Settings.change model.key settings
                            )

                        Just urlSettings ->
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

        Randomize ->
            let
                ( randomPermutations, newSeed ) =
                    Random.step
                        (PermutationGrid.random (Colors.count model.settings.initialVariables))
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

        ShowHelpInfo bool ->
            ( { model | showHelp = bool }, Cmd.none )

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


view : Model -> Browser.Document Msg
view model =
    { title = "Fraquilt"
    , body =
        case model.tutorial of
            Just page ->
                [ Tutorial.view page |> Html.map TutorialMsg ]

            Nothing ->
                [ Html.div
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
                        Settings.viewEditSettings model.selectionState model.settings
                    ]
                ]
    }


type alias Flags =
    { randomSeed : Int }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        appUrl =
            AppUrl.fromUrl url

        -- Todo: handle decode error
        ( settings, randomSeed ) =
            case Settings.fromUrl appUrl of
                Just urlSettings ->
                    ( urlSettings, Random.initialSeed flags.randomSeed )

                Nothing ->
                    let
                        ( permutations, seed ) =
                            Random.step
                                (PermutationGrid.random 3)
                                (Random.initialSeed flags.randomSeed)
                    in
                    ( { level = 9
                      , initialVariables = Colors.init3
                      , permutations = permutations
                      }
                    , seed
                    )

        model =
            { settings = settings
            , key = key
            , randomSeed = randomSeed
            , selectionState = NoneSelected
            , showHelp = False
            , tutorial = Nothing
            }
    in
    case appUrl.path of
        [ "fraquilt", "tutorial" ] ->
            ( { model | tutorial = Just Tutorial.Intro }
            , Cmd.none
            )

        [ "fraquilt", "tutorial", pageNumber ] ->
            ( { model | tutorial = Just (Tutorial.initPage pageNumber) }
            , Cmd.none
            )

        _ ->
            ( model
            , Settings.change key settings
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
