module Main exposing (..)

import Browser
import Color
import Dict exposing (Dict)
import Html exposing (Html, b, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }


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


type alias Style =
    ( String, String )


constructStyles : (config -> List Style) -> Adjustments config -> Int -> config -> String
constructStyles makeStyles adjustments levelA configA =
    let
        configValueList =
            bottomConfigValues adjustments levelA configA
    in
    mapStylesToStringTCO makeStyles configValueList


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


type alias A =
    { r : Int, g : Int, b : Int }


adj =
    { tl = \{ r, g, b } -> { r = g, g = b, b = r }
    , tr = \{ r, g, b } -> { r = b, g = r, b = g }
    , bl = \{ r, g, b } -> { r = r, g = b, b = r }
    , br = \{ r, g, b } -> { r = r, g = g, b = b }
    }


bot { r, g, b } =
    Html.Attributes.style "background-color"
        (Color.toCssString (Color.rgb255 r g b))


toStr : A -> String
toStr { r, g, b } =
    String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b


fromStr : String -> A
fromStr str =
    case String.split "," str of
        [ r, g, b ] ->
            { r = String.toInt r |> Maybe.withDefault 0
            , g = String.toInt g |> Maybe.withDefault 0
            , b = String.toInt b |> Maybe.withDefault 0
            }

        _ ->
            { r = 0, g = 0, b = 0 }


view : Model -> Html ()
view model =
    let
        _ =
            Debug.log "View for level:" model

        level =
            8
    in
    div []
        [ Keyed.node "style"
            []
            [ ( String.fromInt model
              , Keyed.node "style"
                    []
                    [ ( String.fromInt model
                      , Html.text
                            (constructStyles
                                (\{ r, g, b } ->
                                    [ ( "background-color"
                                      , "rgb(" ++ String.join "," [ String.fromInt r, String.fromInt g, String.fromInt b ] ++ ")"
                                      )
                                    ]
                                )
                                adj
                                level
                                { r = 100, g = model, b = 20 }
                            )
                      )
                    ]
              )
            ]
        , lazy3 frameWork level "path" "outer"
        , button [ onClick () ] [ text "Next size" ]
        ]



--


type alias Model =
    Int


update : () -> Model -> ( Model, Cmd msg )
update _ model =
    ( model + 10, Cmd.none )


main : Program () Model ()
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
