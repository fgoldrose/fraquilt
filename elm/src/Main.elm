module Main exposing (..)

import Browser
import Browser.Events
import Dict
import Html exposing (Html, a, div)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy
import Json.Decode
import List.Extra as List
import Random
import Task


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }


type alias Config =
    List Int


configToRbgString : Config -> String
configToRbgString list =
    case list of
        r :: g :: b :: _ ->
            "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"

        _ ->
            "rgb(0,0,0)"


borderWidthString : Int -> Maybe Int -> String
borderWidthString level i =
    i
        |> Maybe.withDefault 0
        |> toFloat
        |> (\x -> ((x / 255 * 6) |> String.fromFloat) ++ "px")


borderRadiusString : Maybe Int -> String
borderRadiusString i =
    i
        |> Maybe.withDefault 0
        |> toFloat
        |> (\x -> ((x / 255 * 100) |> String.fromFloat) ++ "%")


type alias Memoized =
    Dict.Dict Config
        { adjust :
            { tl : Config
            , tr : Config
            , bl : Config
            , br : Config
            }
        , levelImages : Dict.Dict Int (Html Msg)
        }


generateImage : Adjustments Config -> Memoized -> Int -> String -> String -> Config -> ( Html Msg, Memoized )
generateImage adjustments memoized level pathKey currentPosition config =
    if level == 0 then
        ( div
            [ class "box"
            , class currentPosition
            , id pathKey
            , Html.Attributes.style "background-color" (configToRbgString config)
            , Html.Attributes.style "border-top-left-radius" (List.getAt 3 config |> borderRadiusString)
            , Html.Attributes.style "border-top-right-radius" (List.getAt 4 config |> borderRadiusString)
            , Html.Attributes.style "border-bottom-left-radius" (List.getAt 5 config |> borderRadiusString)
            , Html.Attributes.style "border-bottom-right-radius" (List.getAt 6 config |> borderRadiusString)
            ]
            []
        , memoized
        )

    else
        let
            wrapImages subImages =
                Keyed.node "div"
                    [ class "box"
                    , class currentPosition
                    , Html.Attributes.style "background-color" (configToRbgString config)
                    ]
                    [ ( pathKey ++ "-outer"
                      , Keyed.node "div"
                            [ class "outer"
                            , Html.Attributes.style "border-top-left-radius" (List.getAt 3 config |> borderRadiusString)
                            , Html.Attributes.style "border-top-right-radius" (List.getAt 4 config |> borderRadiusString)
                            , Html.Attributes.style "border-bottom-left-radius" (List.getAt 5 config |> borderRadiusString)
                            , Html.Attributes.style "border-bottom-right-radius" (List.getAt 6 config |> borderRadiusString)
                            ]
                            subImages
                      )
                    ]

            generateImageLevel configs =
                let
                    ( tlImage, memoized2 ) =
                        generateImage adjustments
                            memoized
                            (level - 1)
                            (pathKey ++ "-tl")
                            "tl"
                            configs.tl

                    ( trImage, memoized3 ) =
                        generateImage adjustments
                            memoized2
                            (level - 1)
                            (pathKey ++ "-tr")
                            "tr"
                            configs.tr

                    ( blImage, memoized4 ) =
                        generateImage adjustments
                            memoized3
                            (level - 1)
                            (pathKey ++ "-bl")
                            "bl"
                            configs.bl

                    ( brImage, memoized5 ) =
                        generateImage adjustments
                            memoized4
                            (level - 1)
                            (pathKey ++ "-br")
                            "br"
                            configs.br
                in
                ( wrapImages
                    [ ( pathKey ++ "-tl", tlImage )
                    , ( pathKey ++ "-tr", trImage )
                    , ( pathKey ++ "-bl", blImage )
                    , ( pathKey ++ "-br", brImage )
                    ]
                , memoized5
                )
        in
        case Dict.get config memoized of
            Just { adjust, levelImages } ->
                case Dict.get level levelImages of
                    Just image ->
                        ( image, memoized )

                    Nothing ->
                        let
                            ( image, returnedMemoized ) =
                                generateImageLevel adjust

                            newMemoized =
                                Dict.insert config
                                    { adjust = adjust
                                    , levelImages = Dict.insert level image levelImages
                                    }
                                    returnedMemoized
                        in
                        ( image, newMemoized )

            Nothing ->
                let
                    adjust =
                        { tl = adjustments.tl config
                        , tr = adjustments.tr config
                        , bl = adjustments.bl config
                        , br = adjustments.br config
                        }

                    ( image, returnedMemoized ) =
                        generateImageLevel adjust

                    newMemoized =
                        Dict.insert config
                            { adjust = adjust
                            , levelImages = Dict.singleton level image
                            }
                            returnedMemoized
                in
                ( image, newMemoized )


randomListShuffleFunction : Int -> Random.Generator (List Int -> List Int)
randomListShuffleFunction listLength =
    Random.list listLength (Random.int 0 (listLength - 1))
        |> Random.map
            (\listOfIndices ->
                \listInput ->
                    List.indexedMap
                        (\index item ->
                            let
                                swapInput =
                                    List.getAt index listOfIndices
                                        |> Maybe.withDefault index
                            in
                            List.getAt swapInput listInput
                                |> Maybe.withDefault item
                        )
                        listInput
            )


randomizeAdjustments : Int -> Random.Generator (Adjustments Config)
randomizeAdjustments listLength =
    let
        randomList =
            randomListShuffleFunction listLength
    in
    Random.map4 Adjustments
        randomList
        randomList
        randomList
        randomList


randomVariables : Int -> Random.Generator Config
randomVariables n =
    Random.list n (Random.int 0 255)


viewFrameworks : Model -> List ( String, Html Msg )
viewFrameworks model =
    [ ( String.fromInt model.iteration
      , div
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "left" "0"
            ]
            [ generateImage
                model.adjustments
                Dict.empty
                maxLevel
                ("level-" ++ String.fromInt maxLevel)
                "outer"
                model.initialVariables
                |> Tuple.first
            ]
      )
    ]


view : Model -> Html Msg
view model =
    div []
        [ Keyed.node "div"
            [ id "container"
            , onClick
                Randomize
            ]
            (viewFrameworks model)
        ]



--


type alias Model =
    { iteration : Int
    , adjustments : Adjustments Config
    , level : Int
    , initialVariables : Config
    , randomSeed : Random.Seed
    , numberOfVariables : Int -- Length of list
    , levelAnimationDirection : Direction
    , doNextAnimationFrame : List Msg
    }


type Direction
    = Up
    | Down
    | None


type alias Flags =
    { randomSeed : Int }


maxLevel : Int
maxLevel =
    7


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        numberOfVariables =
            8

        level =
            maxLevel

        seed =
            Random.initialSeed flags.randomSeed

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments
    in
    ( { iteration = 0
      , adjustments = adjustments
      , level = level
      , initialVariables = newInitialColor
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      , levelAnimationDirection = Up
      , doNextAnimationFrame = []
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | AnimateLevel
    | DoNextAnimationFrame Msg
    | GotNextAnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            let
                ( randomizedAdjustments, seedAfterAdustments ) =
                    Random.step (randomizeAdjustments model.numberOfVariables) model.randomSeed

                ( newInitialColor, newSeed ) =
                    Random.step (randomVariables model.numberOfVariables) seedAfterAdustments
            in
            ( { model
                | adjustments = randomizedAdjustments
                , initialVariables = newInitialColor
                , randomSeed = newSeed
                , iteration = model.iteration + 1
              }
            , Cmd.none
            )

        AnimateLevel ->
            let
                changeLevel dir m =
                    case dir of
                        Up ->
                            { m | level = m.level + 1 }

                        Down ->
                            { m | level = m.level - 1 }

                        None ->
                            m
            in
            if model.level == maxLevel then
                ( if model.levelAnimationDirection == None then
                    { model
                        | levelAnimationDirection = Down
                        , doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                    }
                        |> changeLevel Down

                  else
                    { model
                        | levelAnimationDirection = None
                    }
                , Cmd.none
                )

            else if model.level == -1 then
                case
                    model.levelAnimationDirection
                of
                    Down ->
                        ( { model
                            | levelAnimationDirection = Up
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( { model
                            | levelAnimationDirection = Up
                            , doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                          }
                            |> changeLevel Up
                        , Cmd.none
                        )

            else
                ( { model
                    | doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                  }
                    |> changeLevel model.levelAnimationDirection
                , Cmd.none
                )

        DoNextAnimationFrame doMsg ->
            ( { model
                | doNextAnimationFrame =
                    model.doNextAnimationFrame ++ [ doMsg ]
              }
            , Cmd.none
            )

        GotNextAnimationFrame ->
            case model.doNextAnimationFrame of
                first :: rest ->
                    ( { model | doNextAnimationFrame = rest }
                    , Task.perform identity (Task.succeed first)
                    )

                _ ->
                    ( model, Cmd.none )


onTransitionEnd : Msg -> Html.Attribute Msg
onTransitionEnd msg =
    Html.Events.on "transitionend" (Json.Decode.succeed msg)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \{ doNextAnimationFrame } ->
                if List.isEmpty doNextAnimationFrame then
                    Sub.none

                else
                    Browser.Events.onAnimationFrameDelta
                        (\_ ->
                            GotNextAnimationFrame
                        )
        }
