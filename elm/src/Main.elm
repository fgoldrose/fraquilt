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
import Random.List
import Task
import Time


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


configToBorderStyle : Config -> List (Html.Attribute Msg)
configToBorderStyle list =
    case list of
        l :: r :: t :: b :: _ ->
            [ -- Html.Attributes.style "border-left-width" (String.fromInt l ++ "px")
              Html.Attributes.style "border-top-left-radius" (String.fromInt r ++ "%")

            --, Html.Attributes.style "border-right-width" (String.fromInt r ++ "px")
            , Html.Attributes.style "border-bottom-right-radius" (String.fromInt r ++ "%")

            -- , Html.Attributes.style "border-top-width" (String.fromInt t ++ "px")
            , Html.Attributes.style "border-top-right-radius" (String.fromInt t ++ "%")

            --, Html.Attributes.style "border-bottom-width" (String.fromInt b ++ "px")
            , Html.Attributes.style "border-bottom-left-radius" (String.fromInt b ++ "%")
            ]

        _ ->
            []


borderWidthString : Maybe Int -> String
borderWidthString i =
    i
        |> Maybe.withDefault 0
        |> toFloat
        |> (\x -> ((x / 255 * 3) |> String.fromFloat) ++ "px")


percentString : Maybe Int -> String
percentString i =
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

        -- , levelImages : Dict.Dict Int (List (Html Msg))
        }


type alias ConfigParams =
    { config : Config
    , adjustments : Adjustments Config
    , memoized : Memoized
    }


generateImage : ConfigParams -> ConfigParams -> Int -> String -> String -> ( Html Msg, ConfigParams, ConfigParams )
generateImage colorConfigParams borderConfigParams level pathKey currentPosition =
    if level == 0 then
        ( div
            ([ class "box"
             , class currentPosition
             , id pathKey
             , Html.Attributes.style "background-color" (configToRbgString colorConfigParams.config)
             ]
                ++ configToBorderStyle borderConfigParams.config
            )
            []
        , colorConfigParams
        , borderConfigParams
        )

    else
        let
            wrapImages subImages =
                Keyed.node "div"
                    ([ class "box"
                     , class currentPosition

                     --  , Html.Attributes.style "border-style" "solid"
                     --  , Html.Attributes.style "border-color" (configToRbgString colorConfigParams.config)
                     , Html.Attributes.style "background-color" (configToRbgString colorConfigParams.config)
                     ]
                     -- ++ configToBorderStyle borderConfigParams.config
                    )
                    subImages

            adjustColor =
                Dict.get colorConfigParams.config colorConfigParams.memoized
                    |> Maybe.map .adjust
                    |> Maybe.withDefault
                        { tl = colorConfigParams.adjustments.tl colorConfigParams.config
                        , tr = colorConfigParams.adjustments.tr colorConfigParams.config
                        , bl = colorConfigParams.adjustments.bl colorConfigParams.config
                        , br = colorConfigParams.adjustments.br colorConfigParams.config
                        }

            adjustBorder =
                Dict.get borderConfigParams.config borderConfigParams.memoized
                    |> Maybe.map .adjust
                    |> Maybe.withDefault
                        { tl = borderConfigParams.adjustments.tl borderConfigParams.config
                        , tr = borderConfigParams.adjustments.tr borderConfigParams.config
                        , bl = borderConfigParams.adjustments.bl borderConfigParams.config
                        , br = borderConfigParams.adjustments.br borderConfigParams.config
                        }

            newColorConfigParams =
                { colorConfigParams | memoized = Dict.insert colorConfigParams.config { adjust = adjustColor } colorConfigParams.memoized }

            newBorderConfigParams =
                { borderConfigParams | memoized = Dict.insert borderConfigParams.config { adjust = adjustBorder } borderConfigParams.memoized }

            ( tlImage, colorMemoized2, borderMemoized2 ) =
                generateImage
                    { newColorConfigParams | config = adjustColor.tl }
                    { newBorderConfigParams | config = adjustBorder.tl }
                    (level - 1)
                    (pathKey ++ "-tl")
                    "tl"

            ( trImage, colorMemoized3, borderMemoized3 ) =
                generateImage
                    { colorMemoized2 | config = adjustColor.tr }
                    { borderMemoized2 | config = adjustBorder.tr }
                    (level - 1)
                    (pathKey ++ "-tr")
                    "tr"

            ( blImage, colorMemoized4, borderMemoized4 ) =
                generateImage
                    { colorMemoized3 | config = adjustColor.bl }
                    { borderMemoized3 | config = adjustBorder.bl }
                    (level - 1)
                    (pathKey ++ "-bl")
                    "bl"

            ( brImage, colorMemoized5, borderMemoized5 ) =
                generateImage
                    { colorMemoized4 | config = adjustColor.br }
                    { borderMemoized4 | config = adjustBorder.br }
                    (level - 1)
                    (pathKey ++ "-br")
                    "br"
        in
        ( wrapImages
            [ ( pathKey ++ "-tl", tlImage )
            , ( pathKey ++ "-tr", trImage )
            , ( pathKey ++ "-bl", blImage )
            , ( pathKey ++ "-br", brImage )
            ]
        , colorMemoized5
        , borderMemoized5
        )


randomListShuffleFunction : Int -> Random.Generator (List Int -> List Int)
randomListShuffleFunction listLength =
    Random.List.shuffle (List.range 0 (listLength - 1))
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


randomizeColors : Int -> Random.Seed -> ( ConfigParams, Random.Seed )
randomizeColors numberOfVariables seed =
    let
        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, newSeed ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments
    in
    ( { adjustments = adjustments, config = newInitialColor, memoized = Dict.empty }
    , newSeed
    )


randomizeBorder : Int -> Random.Seed -> ( ConfigParams, Random.Seed )
randomizeBorder numberOfVariables seed =
    let
        ( borderAdjustments, seed1 ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitial, seed2 ) =
            Random.step (Random.list 4 (Random.int 0 100)) seed1
    in
    ( { adjustments = borderAdjustments, config = newInitial, memoized = Dict.empty }
    , seed2
    )


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
                model.colorParams
                model.borderParams
                maxLevel
                ("level-" ++ String.fromInt maxLevel)
                "outer"
                |> (\( image, _, _ ) -> image)
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
    , colorParams : ConfigParams
    , borderParams : ConfigParams
    , level : Int
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
    6


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        numberOfVariables =
            4

        level =
            maxLevel

        seed =
            Random.initialSeed flags.randomSeed

        ( colorParams, seed1 ) =
            randomizeColors numberOfVariables seed

        ( borderParams, seed2 ) =
            randomizeBorder 4 seed1
    in
    ( { iteration = 0
      , colorParams = colorParams
      , borderParams = borderParams
      , level = level
      , randomSeed = seed2
      , numberOfVariables = numberOfVariables
      , levelAnimationDirection = Up
      , doNextAnimationFrame = []
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | RandomizeBorder
    | AnimateLevel
    | DoNextAnimationFrame Msg
    | GotNextAnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            let
                ( newColorParams, seed1 ) =
                    randomizeColors model.numberOfVariables model.randomSeed

                ( newBorderParams, seed2 ) =
                    randomizeBorder 4 seed1

                ( _, memoizeColors, memoizeBorders ) =
                    generateImage newColorParams newBorderParams maxLevel ("level-" ++ String.fromInt maxLevel) "outer"
            in
            ( { model
                | colorParams = memoizeColors
                , borderParams = memoizeBorders
                , randomSeed = seed2
                , iteration = model.iteration + 1
              }
            , Cmd.none
            )

        RandomizeBorder ->
            let
                ( borderAdjustments, seed1 ) =
                    Random.step (randomizeAdjustments 4) model.randomSeed

                ( newInitial, seed2 ) =
                    Random.step (Random.list 4 (Random.int 0 100)) seed1
            in
            ( { model
                | borderParams = { adjustments = borderAdjustments, config = newInitial, memoized = Dict.empty }
                , randomSeed = seed2
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
            -- \_ -> Time.every 300 (\_ -> RandomizeBorder)
            \{ doNextAnimationFrame } ->
                if List.isEmpty doNextAnimationFrame then
                    Sub.none

                else
                    Browser.Events.onAnimationFrameDelta
                        (\_ ->
                            GotNextAnimationFrame
                        )
        }
