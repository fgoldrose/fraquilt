module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, b, button, div, input, main_, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import List.Extra as List
import Random
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


mapStylesToStringTCO : (config -> Bool -> List Style) -> List ( String, config ) -> String
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
                        (toStyleString pathKey (getStyles config False)
                            ++ toStyleString (pathKey ++ "-old") (getStyles config True)
                            ++ soFar
                        )
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
            [ div [ class "old-inner", class currentPosition, id (pathKey ++ "-old") ] [] ]

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


rgbToStyles : Config -> Bool -> List Style
rgbToStyles list isOld =
    list
        |> List.indexedMap
            (\variable initialVar ->
                ( "--"
                    ++ addOldPrefix isOld
                    ++ "var-"
                    ++ String.fromInt variable
                , "var(--"
                    ++ addOldPrefix isOld
                    ++ "initial-var-"
                    ++ String.fromInt initialVar
                    ++ ")"
                )
            )


addOldPrefix : Bool -> String
addOldPrefix isOld =
    if isOld then
        "old-"

    else
        ""


initialColorVariables : Config -> Bool -> List Style
initialColorVariables list isOld =
    List.indexedMap
        (\variable value ->
            ( "--"
                ++ addOldPrefix isOld
                ++ "initial-var-"
                ++ String.fromInt variable
            , String.fromInt value
            )
        )
        list



-- [ ( "background-color"
--   , "rgb("
--         ++ String.join ","
--             [ String.fromInt r, String.fromInt g, String.fromInt b ]
--         ++ ")"
--   )
-- ]


view : Model Config -> Html Msg
view model =
    div []
        [ Keyed.node "style"
            [ id (String.fromInt model.iteration) ]
            [ ( String.fromInt model.iteration
              , Html.text
                    (mapStylesToStringTCO
                        rgbToStyles
                        model.configs
                    )
              )
            ]
        , Html.node "style"
            []
            [ Html.text
                (initialColorVariables
                    model.initialVariables
                    False
                    ++ initialColorVariables
                        model.oldInitialVariables
                        True
                    ++ [ ( "--old-opacity", model.oldVarOpacity |> String.fromInt ) ]
                    |> toStyleString ":root"
                )
            ]
        , Keyed.node "div"
            []
            [ ( String.fromInt model.level, lazy3 frameWork model.level "path" "outer" ) ]
        , button [ onClick Randomize ] [ text "Random" ]
        , button [ onClick (ChangeLevel (model.level - 1)) ] [ Html.text "- level" ]
        , button [ onClick (ChangeLevel (model.level + 1)) ] [ Html.text "+ level" ]
        , button [ onClick ChangeStartColor ] [ Html.text "Change start color" ]
        ]



--


type alias Model config =
    { iteration : Int
    , configs : List ( String, config )
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
      , configs = bottomConfigValues adjustments level (List.range 0 (numberOfVariables - 1))
      , adjustments = adjustments
      , level = level
      , initialVariables = newInitialColor |> Debug.log "init color"
      , oldVarOpacity = 1
      , oldInitialVariables = newInitialColor
      , randomSeed = seedAfterColor
      , numberOfVariables = numberOfVariables
      }
    , Cmd.none
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
                , configs =
                    bottomConfigValues
                        updatedModel.adjustments
                        updatedModel.level
                        (List.range 0 (updatedModel.numberOfVariables - 1))
            }
    in
    case msg of
        Randomize ->
            let
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
                swapTwoVars m =
                    if m.oldVarOpacity == 1 then
                        { m
                            | initialVariables = newInitialColor
                            , oldVarOpacity = 0
                        }

                    else
                        { m
                            | oldVarOpacity = 1
                            , oldInitialVariables = newInitialColor
                        }

                ( newInitialColor, newSeed ) =
                    Random.step (randomizeColor model.numberOfVariables)
                        model.randomSeed
            in
            ( { model
                | randomSeed = newSeed
              }
                |> swapTwoVars
            , Cmd.none
            )

        ChangeLevel i ->
            ( { model
                | level = i
              }
                |> recalcConfigs
            , Cmd.none
            )


main : Program Flags (Model Config) Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none --Time.every 3000 (\_ -> ChangeStartColor)
        }
