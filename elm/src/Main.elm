module Main exposing (..)

import Browser
import Color
import Html exposing (Html, b, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)


type alias Adjustments a =
    { tl : a -> a
    , tr : a -> a
    , bl : a -> a
    , br : a -> a
    }



-- recursiveView : (config -> String) -> (String -> config) -> (config -> Html.Attribute msg) -> Adjustments config -> Int -> config -> Html msg
-- recursiveView toString fromString bottom adjustments levelA configA =
--     let
--         specificHelper : Int -> String -> Html msg
--         specificHelper level configString =
--             let
--                 _ =
--                     Debug.log "level" ( level, configString )
--                 config =
--                     fromString configString
--             in
--             if level == 0 then
--                 div [ class "inner", bottom config ] []
--             else
--                 div []
--                     [ div
--                         [ class "box tl"
--                         ]
--                         [ lazy2 specificHelper
--                             (level - 1)
--                             (toString (adjustments.tl config))
--                         ]
--                     , div [ class "box tr" ]
--                         [ lazy2 specificHelper
--                             (level - 1)
--                             (toString (adjustments.tr config))
--                         ]
--                     , div [ class "box bl" ]
--                         [ lazy2 specificHelper
--                             (level - 1)
--                             (toString (adjustments.bl config))
--                         ]
--                     , div [ class "box br" ]
--                         [ lazy2 specificHelper
--                             (level - 1)
--                             (toString (adjustments.br config))
--                         ]
--                     ]
--     in
--     specificHelper levelA (toString configA)


specificHelper : Int -> String -> Html msg
specificHelper level configString =
    let
        _ =
            Debug.log "level" ( level, configString )

        config =
            fromStr configString
    in
    if level == 0 then
        div [ class "inner", bot config ] []

    else
        div []
            [ div
                [ class "box tl"
                ]
                [ lazy2 specificHelper
                    (level - 1)
                    (toStr (adj.tl config))
                ]
            , div [ class "box tr" ]
                [ lazy2 specificHelper
                    (level - 1)
                    (toStr (adj.tr config))
                ]
            , div [ class "box bl" ]
                [ lazy2 specificHelper
                    (level - 1)
                    (toStr (adj.bl config))
                ]
            , div [ class "box br" ]
                [ lazy2 specificHelper
                    (level - 1)
                    (toStr (adj.br config))
                ]
            ]


type alias A =
    { r : Int, g : Int, b : Int }


adj =
    { tl = \{ r, g, b } -> { r = g, g = b, b = r }
    , tr = identity
    , bl = identity
    , br = identity
    }



-- adj =
--     { tl = identity
--     , tr = identity
--     , bl = identity
--     , br = identity
--     }


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
    in
    div []
        [ div [ class "outer" ]
            [ --recursiveView toStr fromStr bot adj 2 { r = 100, g = 50, b = 20 }
              lazy2 specificHelper model "100,50,20"
            ]
        , button [ onClick () ] [ text "Next size" ]
        ]



--


type alias Model =
    Int


update : () -> Model -> ( Model, Cmd msg )
update _ model =
    ( model + 1, Cmd.none )


main : Program () Model ()
main =
    Browser.element
        { init = \_ -> ( 0, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
