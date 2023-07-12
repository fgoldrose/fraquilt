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


recursiveView : (config -> String) -> (config -> Html.Attribute msg) -> Adjustments config -> Int -> config -> Html msg
recursiveView toString bottom adjustments levelA configA =
    let
        specificHelper : Dict ( Int, String ) (Html msg) -> String -> Int -> config -> ( Html msg, Dict ( Int, String ) (Html msg) )
        specificHelper memoizer pathKey level config =
            case Dict.get ( level, toString config ) memoizer of
                Just html ->
                    -- let
                    --     _ =
                    --         Debug.log "memoized" ( level, config )
                    -- in
                    ( html, memoizer )

                Nothing ->
                    -- let
                    --     _ =
                    --         Debug.log "new" ( level, config )
                    -- in
                    if level == 0 then
                        let
                            html =
                                div [ class "inner", bottom config ] []
                        in
                        ( html
                        , Dict.insert ( 0, toString config ) html memoizer
                        )

                    else
                        let
                            ( tl, memoizer2 ) =
                                specificHelper memoizer
                                    (pathKey ++ "-tl")
                                    (level - 1)
                                    (adjustments.tl config)

                            ( tr, memoizer3 ) =
                                specificHelper memoizer2
                                    (pathKey ++ "-tr")
                                    (level - 1)
                                    (adjustments.tr config)

                            ( bl, memoizer4 ) =
                                specificHelper memoizer3
                                    (pathKey ++ "-bl")
                                    (level - 1)
                                    (adjustments.bl config)

                            ( br, memoizer5 ) =
                                specificHelper memoizer4
                                    (pathKey ++ "-br")
                                    (level - 1)
                                    (adjustments.br config)

                            html =
                                Keyed.node "div"
                                    []
                                    [ ( pathKey ++ "-tl"
                                      , div
                                            [ class "box tl"
                                            ]
                                            [ tl
                                            ]
                                      )
                                    , ( pathKey ++ "-tr"
                                      , div [ class "box tr" ]
                                            [ tr
                                            ]
                                      )
                                    , ( pathKey ++ "-bl"
                                      , div [ class "box bl" ]
                                            [ bl
                                            ]
                                      )
                                    , ( pathKey ++ "-br"
                                      , div [ class "box br" ]
                                            [ br
                                            ]
                                      )
                                    ]
                        in
                        ( html
                        , Dict.insert ( level, toString config ) html memoizer5
                        )
    in
    specificHelper Dict.empty "" levelA configA |> Tuple.first


constructStyles : (String -> config -> String) -> Adjustments config -> Int -> config -> String
constructStyles makeStyleString adjustments levelA configA =
    let
        specificHelper : String -> Int -> config -> String
        specificHelper pathKey level config =
            if level == 0 then
                makeStyleString pathKey config

            else
                specificHelper (pathKey ++ "-tl") (level - 1) (adjustments.tl config)
                    ++ specificHelper (pathKey ++ "-tr") (level - 1) (adjustments.tr config)
                    ++ specificHelper (pathKey ++ "-bl") (level - 1) (adjustments.bl config)
                    ++ specificHelper (pathKey ++ "-br") (level - 1) (adjustments.br config)
    in
    specificHelper "path" levelA configA


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



-- specificHelper : Int -> String -> Html msg
-- specificHelper level configString =
--     let
--         _ =
--             Debug.log "level" ( level, configString )
--         config =
--             fromStr configString
--     in
--     if level == 0 then
--         div
--             [ class "inner", bot config ]
--             []
--     else
--         Keyed.node "div"
--             []
--             [ ( String.fromInt (level - 1) ++ " " ++ toStr (adj.tl config)
--               , div
--                     [ class "box tl"
--                     ]
--                     [ lazy2 specificHelper
--                         (level - 1)
--                         (toStr (adj.tl config))
--                     ]
--               )
--             , ( String.fromInt (level - 1) ++ " " ++ toStr (adj.tr config)
--               , div [ class "box tr" ]
--                     [ lazy2 specificHelper
--                         (level - 1)
--                         (toStr (adj.tr config))
--                     ]
--               )
--             , ( String.fromInt (level - 1) ++ " " ++ toStr (adj.bl config)
--               , div [ class "box bl" ]
--                     [ lazy2 specificHelper
--                         (level - 1)
--                         (toStr (adj.bl config))
--                     ]
--               )
--             , ( String.fromInt (level - 1) ++ " " ++ toStr (adj.br config)
--               , div [ class "box br" ]
--                     [ lazy2 specificHelper
--                         (level - 1)
--                         (toStr (adj.br config))
--                     ]
--               )
--             ]


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
              , Html.text <|
                    Debug.log "styles" <|
                        constructStyles
                            (\pathKey { r, g, b } ->
                                "#"
                                    ++ pathKey
                                    ++ " { background-color: rgb("
                                    ++ String.join ","
                                        [ String.fromInt r, String.fromInt g, String.fromInt b ]
                                    ++ ");} "
                            )
                            adj
                            level
                            { r = 100, g = model, b = 20 }
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
