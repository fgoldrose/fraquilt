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


bottomConfigValues : Adjustments config -> Int -> config -> List ( String, config )
bottomConfigValues adjustments level_ config_ =
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
                            (helper (pathKey ++ "-br") (level - 1) (adjustments.br config) soFar)
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
        div [ class "box", class currentPosition ]
            [ frameWork
                (level - 1)
                (pathKey ++ "-tl")
                "tl"
            , frameWork
                (level - 1)
                (pathKey ++ "-tr")
                "tr"
            , frameWork
                (level - 1)
                (pathKey ++ "-bl")
                "bl"
            , frameWork
                (level - 1)
                (pathKey ++ "-br")
                "br"
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


randomizeColor : Int -> Random.Generator Config
randomizeColor n =
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


view : Model Config -> Html Msg
view model =
    div []
        [ Keyed.node "style"
            [ id (String.fromInt model.iteration) ]
            [ ( String.fromInt model.iteration
              , Html.text model.styleString
              )
            ]
        , Html.node "style"
            []
            [ Html.text
                (initialColorVariables
                    model.initialVariables
                    |> toStyleString "#new"
                )
            , Html.text
                (initialColorVariables
                    model.oldInitialVariables
                    |> toStyleString "#old"
                )
            , Html.text
                ([ ( "--old-opacity", model.oldVarOpacity |> String.fromInt ) ]
                    |> toStyleString ":root"
                )
            ]
        , Keyed.node "div"
            [ onClick Randomize ]
            [ ( String.fromInt model.level
              , div [ id "new" ] [ lazy3 frameWork model.level "path" "outer" ]
              )
            , ( String.fromInt model.level
              , div [ id "old", onTransitionEnd ] [ lazy3 frameWork model.level "path" "outer" ]
              )
            ]

        -- , button [ onClick Randomize ] [ text "Random" ]
        -- , button [ onClick (ChangeLevel (model.level - 1)) ] [ Html.text "- level" ]
        -- , button [ onClick (ChangeLevel (model.level + 1)) ] [ Html.text "+ level" ]
        -- , button [ onClick ChangeStartColor ] [ Html.text "Change start color" ]
        ]



--


type alias Model config =
    { iteration : Int
    , styleString : String
    , adjustments : Adjustments config
    , level : Int
    , initialVariables : config
    , oldVarOpacity : Int
    , oldInitialVariables : config
    , randomSeed : Random.Seed
    , numberOfVariables : Int -- Length of list
    }


type alias Flags =
    { randomSeed : Int }


init : Flags -> ( Model Config, Cmd Msg )
init flags =
    let
        numberOfVariables =
            10

        level =
            7

        seed =
            Random.initialSeed flags.randomSeed

        ( adjustments, seedAfterAdustments ) =
            Random.step (randomizeAdjustments numberOfVariables) seed

        ( newInitialColor, seedAfterColor ) =
            Random.step (randomizeColor numberOfVariables) seedAfterAdustments
    in
    ( { iteration = 0
      , styleString =
            bottomConfigValues adjustments level (List.range 0 (numberOfVariables - 1))
                |> mapStylesToStringTCO rgbToStyles
      , adjustments = adjustments
      , level = level
      , initialVariables = newInitialColor |> Debug.log "init color"
      , oldVarOpacity = 1
      , oldInitialVariables = newInitialColor
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      }
    , Process.sleep 1000
        |> Task.perform (\_ -> ChangeStartColor)
    )


type Msg
    = Randomize
    | ChangeLevel Int
    | ChangeStartColor


update : Msg -> Model Config -> ( Model Config, Cmd msg )
update msg model =
    let
        recalcConfigs updatedModel =
            { updatedModel
                | iteration = model.iteration + 1
                , styleString =
                    bottomConfigValues
                        updatedModel.adjustments
                        updatedModel.level
                        (List.range 0 (updatedModel.numberOfVariables - 1))
                        |> mapStylesToStringTCO rgbToStyles
                        |> (\x ->
                                let
                                    _ =
                                        Debug.log "newStyles" ()
                                in
                                x
                           )
            }
    in
    case msg of
        Randomize ->
            let
                _ =
                    Debug.log "randomize" ()

                ( randomizedAdjustments, seedAfterAdustments ) =
                    Random.step (randomizeAdjustments model.numberOfVariables) model.randomSeed

                ( newInitialColor, newSeed ) =
                    Random.step (randomizeColor model.numberOfVariables) seedAfterAdustments
            in
            ( { model
                | adjustments = randomizedAdjustments
                , randomSeed = newSeed
                , initialVariables = newInitialColor
              }
                |> recalcConfigs
            , Cmd.none
            )

        ChangeStartColor ->
            let
                randomizeVariables vars =
                    Random.step
                        (Random.map2
                            (\index add -> List.setAt index add vars)
                            (Random.int 0
                                (model.numberOfVariables - 1)
                            )
                            (Random.int 0 255)
                        )
                        model.randomSeed
            in
            if model.oldVarOpacity == 1 then
                let
                    ( newColor, newSeed ) =
                        randomizeVariables model.oldInitialVariables
                in
                ( { model
                    | initialVariables = newColor
                    , oldVarOpacity = 0
                    , randomSeed = newSeed
                  }
                , Cmd.none
                )

            else
                let
                    ( newColor, newSeed ) =
                        randomizeVariables model.initialVariables
                in
                ( { model
                    | oldVarOpacity = 1
                    , oldInitialVariables = newColor
                    , randomSeed = newSeed
                  }
                , Cmd.none
                )

        ChangeLevel i ->
            ( { model
                | level = i
              }
                |> recalcConfigs
            , Cmd.none
            )


onTransitionEnd : Html.Attribute Msg
onTransitionEnd =
    Html.Events.on "transitionend" (Json.Decode.succeed ChangeStartColor)


main : Program Flags (Model Config) Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
