module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, b, button, div, input, main_, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Random


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
    helper "path" level_ config_ []


mapStylesToStringTCO : (config -> List Style) -> List ( String, config ) -> String
mapStylesToStringTCO getStyles configValueList =
    let
        toStyleString : String -> List Style -> String
        toStyleString pathKey styles =
            "#"
                ++ pathKey
                ++ " { "
                ++ (List.map (\( key, value ) -> key ++ ": " ++ value ++ ";") styles |> String.join " ")
                ++ " }"

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


frameWork : Int -> String -> String -> Html msg
frameWork level pathKey currentPosition =
    if level == 0 then
        div
            [ class "box", class currentPosition, id pathKey ]
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


type alias RgbColor =
    { r : Int, g : Int, b : Int }


type ColorProperty
    = R
    | G
    | B


randomColorProperty : Random.Generator ColorProperty
randomColorProperty =
    Random.uniform R [ G, B ]


colorPropertyToColor : ColorProperty -> RgbColor -> Int
colorPropertyToColor colorProperty { r, g, b } =
    case colorProperty of
        R ->
            r

        G ->
            g

        B ->
            b


swap : ColorProperty -> ColorProperty -> RgbColor -> RgbColor
swap swapColor newColorProperty color =
    case swapColor of
        R ->
            { color | r = colorPropertyToColor newColorProperty color }

        G ->
            { color | b = colorPropertyToColor newColorProperty color }

        B ->
            { color | g = colorPropertyToColor newColorProperty color }


randomizeAdjustments : Random.Generator (Adjustments RgbColor)
randomizeAdjustments =
    let
        swapAll r g b =
            -- let
            --     _ =
            --         Debug.log "r" r
            --     _ =
            --         Debug.log "g" g
            --     _ =
            --         Debug.log "b" b
            -- in
            swap R r >> swap G g >> swap B b
    in
    Random.map4 Adjustments
        (Random.map3 swapAll
            randomColorProperty
            randomColorProperty
            randomColorProperty
        )
        (Random.map3 swapAll randomColorProperty randomColorProperty randomColorProperty)
        (Random.map3 swapAll randomColorProperty randomColorProperty randomColorProperty)
        (Random.map3 swapAll randomColorProperty randomColorProperty randomColorProperty)


randomizeColor : Random.Generator RgbColor
randomizeColor =
    -- Not actually random cause it's better to have a range when swapping for more interesting variations
    Random.map3 RgbColor
        (Random.int 0 85)
        (Random.int 85 170)
        (Random.int 170 255)


rgbToStyles : RgbColor -> List Style
rgbToStyles { r, g, b } =
    [ ( "background-color"
      , "rgb(" ++ String.join "," [ String.fromInt r, String.fromInt g, String.fromInt b ] ++ ")"
      )
    ]


view : Model RgbColor -> Html Msg
view model =
    let
        _ =
            Debug.log "View for level:" model.level
    in
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
        , Keyed.node "div"
            []
            [ ( String.fromInt model.level, lazy3 frameWork model.level "path" "outer" ) ]
        , button [ onClick Randomize ] [ text "Random" ]
        , button [ onClick (ChangeLevel (model.level - 1)) ] [ Html.text "- level" ]
        , button [ onClick (ChangeLevel (model.level + 1)) ] [ Html.text "+ level" ]
        ]



--


type alias Model config =
    { iteration : Int
    , configs : List ( String, config )
    , adjustments : Adjustments config
    , level : Int
    , initConfig : config
    , randomSeed : Random.Seed
    }


type alias Flags =
    { randomSeed : Int }


init : Flags -> ( Model RgbColor, Cmd Msg )
init flags =
    let
        level =
            7

        seed =
            Random.initialSeed flags.randomSeed

        ( adjustments, seedAfterAdustments ) =
            Random.step randomizeAdjustments seed

        ( newInitialColor, seedAfterColor ) =
            Random.step randomizeColor seedAfterAdustments
    in
    ( { iteration = 0
      , configs = bottomConfigValues adjustments level newInitialColor
      , adjustments = adjustments
      , level = level
      , initConfig = newInitialColor |> Debug.log "init color"
      , randomSeed = seedAfterColor
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | ChangeLevel Int


update : Msg -> Model RgbColor -> ( Model RgbColor, Cmd msg )
update msg model =
    let
        recalcConfigs updatedModel =
            { updatedModel
                | iteration = model.iteration + 1
                , configs =
                    bottomConfigValues
                        updatedModel.adjustments
                        updatedModel.level
                        updatedModel.initConfig
            }
    in
    case msg of
        Randomize ->
            let
                ( randomizedAdjustments, seedAfterAdustments ) =
                    Random.step randomizeAdjustments model.randomSeed

                ( newInitialColor, newSeed ) =
                    Random.step randomizeColor seedAfterAdustments
            in
            ( { model
                | adjustments = randomizedAdjustments
                , randomSeed = newSeed
                , initConfig = newInitialColor
              }
                |> recalcConfigs
            , Cmd.none
            )

        ChangeLevel i ->
            ( { model
                | level = i
              }
                |> recalcConfigs
            , Cmd.none
            )


main : Program Flags (Model RgbColor) Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
