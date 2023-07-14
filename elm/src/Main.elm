module Main exposing (..)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, b, button, div, input, main_, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Json.Decode
import List.Extra as List
import Process
import Random
import Task
import Time


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }


type alias Style =
    ( String, String )


allConfigValues : Adjustments config -> Int -> config -> List ( String, config )
allConfigValues adjustments level_ config_ =
    let
        helper : String -> Int -> config -> List ( String, config ) -> List ( String, config )
        helper pathKey level config soFar =
            if level == 0 then
                ( pathKey, config ) :: soFar

            else
                helper (pathKey ++ "-tl")
                    (level - 1)
                    (adjustments.tl config)
                    (helper (pathKey ++ "-tr")
                        (level - 1)
                        (adjustments.tr config)
                        (helper (pathKey ++ "-bl")
                            (level - 1)
                            (adjustments.bl config)
                            (helper (pathKey ++ "-br")
                                (level - 1)
                                (adjustments.br config)
                                (( pathKey, config ) :: soFar)
                            )
                        )
                    )
    in
    helper "#path" level_ config_ []


mapStylesToStringTCO : (config -> List Style) -> List ( String, config ) -> String
mapStylesToStringTCO getStyles configValueList =
    let
        helper : List ( String, config ) -> String -> String
        helper configValues soFar =
            case configValues of
                [] ->
                    soFar

                ( pathKey, config ) :: rest ->
                    helper
                        rest
                        (toStyleString pathKey (getStyles config) ++ soFar)
    in
    helper configValueList ""


toStyleString : String -> List Style -> String
toStyleString pathKey styles =
    pathKey
        ++ " { "
        ++ (List.map (\( key, value ) -> key ++ ": " ++ value ++ ";") styles |> String.join " ")
        ++ " }"


frameWork : Int -> String -> String -> Html msg
frameWork level pathKey currentPosition =
    if level == 0 then
        div
            [ class "box inner", class currentPosition, id pathKey ]
            []

    else
        Keyed.node "div"
            [ class "box", class currentPosition, class ("level-" ++ String.fromInt level), id pathKey ]
            [ ( pathKey ++ "-tl"
              , frameWork
                    (level - 1)
                    (pathKey ++ "-tl")
                    "tl"
              )
            , ( pathKey ++ "-tr"
              , frameWork
                    (level - 1)
                    (pathKey ++ "-tr")
                    "tr"
              )
            , ( pathKey ++ "-bl"
              , frameWork
                    (level - 1)
                    (pathKey ++ "-bl")
                    "bl"
              )
            , ( pathKey ++ "-br"
              , frameWork
                    (level - 1)
                    (pathKey ++ "-br")
                    "br"
              )
            ]


type alias Config =
    List Int


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


rgbToStyles : Config -> List Style
rgbToStyles list =
    list
        |> List.indexedMap
            (\variable initialVar ->
                ( "--var-" ++ String.fromInt variable
                , "var(--initial-var-" ++ String.fromInt initialVar ++ ")"
                )
            )


initialColorVariables : Config -> List Style
initialColorVariables list =
    List.indexedMap
        (\variable value ->
            ( "--initial-var-" ++ String.fromInt variable
            , String.fromInt value
            )
        )
        list


viewFrameworks : String -> Direction -> Fraquilt -> Html Msg
viewFrameworks idString direction image =
    div
        [ id idString ]
        [ div
            [ -- , onTransitionEnd (AnimateLevel level)
              -- , Html.Attributes.style "opacity"
              --     (if level <= image.level then
              --         "1"
              --      else
              --         "0"
              --     )
              --, Html.Attributes.style "transition" "opacity 0.1s linear"
              Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "0"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "left" "0"
            ]
            [ lazy3 frameWork maxLevel "path" "outer" ]
        ]



-- (List.range 0 maxLevel
--     |> List.map
--         (\level ->
--             ( String.fromInt level
--             , div
--                 [ id ("level-" ++ String.fromInt level)
--                 -- , onTransitionEnd (AnimateLevel level)
--                 , Html.Attributes.style "opacity"
--                     (if level <= image.level then
--                         "1"
--                      else
--                         "0"
--                     )
--                 --, Html.Attributes.style "transition" "opacity 0.1s linear"
--                 , Html.Attributes.style "position" "absolute"
--                 , Html.Attributes.style "top" "0"
--                 , Html.Attributes.style "bottom" "0"
--                 , Html.Attributes.style "right" "0"
--                 , Html.Attributes.style "left" "0"
--                 ]
--                 [ lazy3 frameWork level "path" "outer" ]
--             )
--         )
-- )


view : Model -> Html Msg
view model =
    div []
        [ Keyed.node "style"
            [ id (String.fromInt model.newImage.iteration ++ "-" ++ String.fromInt model.oldImage.iteration) ]
            [ ( String.fromInt model.newImage.iteration ++ "-" ++ String.fromInt model.oldImage.iteration
              , Html.text (model.oldImage.styleString ++ model.newImage.styleString)
              )
            ]
        , Html.node "style"
            []
            [ Html.text
                (initialColorVariables
                    model.newImage.initialVariables
                    |> toStyleString "#new"
                )
            , Html.text
                (initialColorVariables
                    model.oldImage.initialVariables
                    |> toStyleString "#old"
                )
            , Html.text
                ([ ( "--old-opacity", model.oldVarOpacity |> String.fromInt ) ]
                    |> toStyleString ":root"
                )
            , Html.text
                ((List.range 0 maxLevel
                    |> List.map
                        (\level ->
                            [ ( "opacity"
                              , if (maxLevel - level) <= model.newImage.level then
                                    "1"

                                else
                                    "0"
                              )
                            ]
                                |> toStyleString (".level-" ++ String.fromInt level)
                        )
                 )
                    |> String.join " "
                )
            ]
        , div
            [ id "container"
            , onClick AnimateLevel
            ]
            [ viewFrameworks "new" model.levelAnimationDirection model.newImage ]

        -- , button [ onClick (DoNextAnimationFrame Randomize) ] [ text "Random" ]
        -- , button [ onClick (ChangeLevel -1) ] [ Html.text "- level" ]
        -- , button [ onClick (ChangeLevel 1) ] [ Html.text "+ level" ]
        -- , button [ onClick (DoNextAnimationFrame ChangeStartColor) ] [ Html.text "Change start color" ]
        ]



--


type alias Fraquilt =
    { iteration : Int
    , styleString : String
    , adjustments : Adjustments Config
    , level : Int
    , initialVariables : Config
    }


type alias Model =
    { newImage : Fraquilt
    , oldImage : Fraquilt
    , oldVarOpacity : Int
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
            10

        level =
            0

        seed =
            Random.initialSeed flags.randomSeed

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments

        initImage : Fraquilt
        initImage =
            { iteration = 0
            , styleString =
                allConfigValues adjustments maxLevel (List.range 0 (numberOfVariables - 1))
                    |> mapStylesToStringTCO rgbToStyles
            , adjustments = adjustments
            , level = level
            , initialVariables = newInitialColor |> Debug.log "init color"
            }
    in
    ( { newImage = initImage
      , oldImage = initImage
      , oldVarOpacity = 1
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      , levelAnimationDirection = None
      , doNextAnimationFrame = []
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | AnimateLevel
    | ChangeLevel Int
    | ChangeStartColor
    | DoNextAnimationFrame Msg
    | GotNextAnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "level" ( msg, model.newImage.level )
    in
    case msg of
        Randomize ->
            let
                ( updateRandomize, newSeed ) =
                    randomizeImage model.numberOfVariables model.randomSeed
            in
            ( { model | randomSeed = newSeed }
                |> updateImage updateRandomize
            , Cmd.none
            )

        ChangeStartColor ->
            let
                ( updateVars, newSeed ) =
                    Random.step
                        (Random.map2
                            (\index add -> List.setAt index add)
                            (Random.int 0
                                (model.numberOfVariables - 1)
                            )
                            (Random.int 0 255)
                        )
                        model.randomSeed

                updateImageFunc image =
                    { image | initialVariables = updateVars image.initialVariables }
            in
            ( { model | randomSeed = newSeed }
                |> updateImage updateImageFunc
            , Cmd.none
            )

        AnimateLevel ->
            let
                currentImage =
                    getCurrentImage model

                changeLevel dir image =
                    case dir of
                        Up ->
                            { image | level = image.level + 1 }

                        Down ->
                            { image | level = image.level - 1 }

                        None ->
                            image
            in
            ( if currentImage.level == maxLevel then
                if model.levelAnimationDirection == None then
                    { model
                        | levelAnimationDirection = Down
                        , doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                    }
                        |> updateImage (changeLevel Down)

                else
                    { model
                        | levelAnimationDirection = None
                    }

              else if currentImage.level == 0 then
                case
                    model.levelAnimationDirection
                of
                    Down ->
                        { model
                            | levelAnimationDirection = None
                        }

                    None ->
                        { model
                            | levelAnimationDirection = Up
                            , doNextAnimationFrame = [ Randomize, AnimateLevel ]
                        }

                    Up ->
                        { model
                            | doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                        }
                            |> updateImage (changeLevel Up)

              else
                { model
                    | doNextAnimationFrame = model.doNextAnimationFrame ++ [ AnimateLevel ]
                }
                    |> updateImage (changeLevel model.levelAnimationDirection)
            , Cmd.none
            )

        ChangeLevel i ->
            ( model
                |> updateImage
                    (\image ->
                        { image
                            | level = image.level + i
                        }
                    )
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


getCurrentImage : Model -> Fraquilt
getCurrentImage model =
    -- if model.oldVarOpacity == 1 then
    --     model.oldImage
    -- else
    --     model.newImage
    model.newImage


updateImage : (Fraquilt -> Fraquilt) -> Model -> Model
updateImage f model =
    -- if model.oldVarOpacity == 1 then
    --     let
    --         updatedImage =
    --             f model.oldImage
    --     in
    --     { model
    --         | newImage = updatedImage
    --         , oldVarOpacity = 0
    --     }
    -- else
    --     let
    --         updatedImage =
    --             f model.newImage
    --     in
    --     { model
    --         | oldVarOpacity = 1
    --         , oldImage = updatedImage
    --     }
    { model | newImage = f model.newImage }


recalcConfigs : Fraquilt -> Fraquilt
recalcConfigs image =
    { image
        | iteration = image.iteration + 1
        , styleString =
            allConfigValues
                image.adjustments
                maxLevel
                (List.range 0 (List.length image.initialVariables - 1))
                |> mapStylesToStringTCO rgbToStyles
                |> (\x ->
                        let
                            _ =
                                Debug.log "newStyles" ()
                        in
                        x
                   )
    }


randomizeImage : Int -> Random.Seed -> ( Fraquilt -> Fraquilt, Random.Seed )
randomizeImage numberOfVariables randomSeed =
    let
        _ =
            Debug.log "randomize" ()

        ( randomizedAdjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) randomSeed

        ( newInitialColor, newSeed ) =
            Random.step (randomVariables numberOfVariables) seedAfterAdustments
    in
    ( \image ->
        { image
            | adjustments = randomizedAdjustments
            , initialVariables = newInitialColor
        }
            |> recalcConfigs
    , newSeed
    )


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
                        (\x ->
                            let
                                _ =
                                    Debug.log "onAnimationFrameDelta" x
                            in
                            GotNextAnimationFrame
                        )
        }
