module Routing exposing (..)

import AppUrl exposing (AppUrl, QueryParameters)
import Colors exposing (InitialVariables)
import Dict exposing (Dict)
import Permutation exposing (Permutation)
import PermutationGrid exposing (PermutationGrid)
import Url exposing (Url)


type Route
    = App (Maybe UrlSettings)
    | Tutorial TutorialRoute


type TutorialRoute
    = Tutorial1
    | Tutorial2
    | Tutorial3
    | Tutorial4
    | TutorialEnd


parseUrl : Url -> Route
parseUrl url =
    let
        fragmentAsPath =
            { url | path = url.fragment |> Maybe.withDefault "" }

        appUrl =
            AppUrl.fromUrl fragmentAsPath
    in
    case appUrl.path of
        [ "tutorial", "1" ] ->
            Tutorial Tutorial1

        [ "tutorial", "2" ] ->
            Tutorial Tutorial2

        [ "tutorial", "3" ] ->
            Tutorial Tutorial3

        [ "tutorial", "4" ] ->
            Tutorial Tutorial4

        [ "tutorial", "end" ] ->
            Tutorial TutorialEnd

        _ ->
            App (settingsFromQueryParams appUrl.queryParameters)


type alias UrlSettings =
    { level : Int
    , initialVariables : InitialVariables
    , permutations : PermutationGrid
    }


reverse : Route -> String
reverse route =
    let
        appUrl =
            case route of
                App maybeSettings ->
                    { path = [ "fraquilt", "" ]
                    , fragment = Nothing
                    , queryParameters =
                        maybeSettings
                            |> Maybe.map settingsToQueryParams
                            |> Maybe.withDefault Dict.empty
                    }

                Tutorial tutorialRoute ->
                    let
                        fragment =
                            case tutorialRoute of
                                Tutorial1 ->
                                    "/tutorial/1"

                                Tutorial2 ->
                                    "/tutorial/2"

                                Tutorial3 ->
                                    "/tutorial/3"

                                Tutorial4 ->
                                    "/tutorial/4"

                                TutorialEnd ->
                                    "/tutorial/end"
                    in
                    { path = [ "fraquilt", "" ]
                    , fragment = Just fragment
                    , queryParameters = Dict.empty
                    }
    in
    AppUrl.toString appUrl


settingsToQueryParams : UrlSettings -> QueryParameters
settingsToQueryParams settings =
    Dict.fromList
        [ ( "level", [ String.fromInt settings.level ] )
        , ( "colors", [ Colors.toUrlString settings.initialVariables ] )
        , ( "tl", [ Permutation.toUrlString settings.permutations.tl ] )
        , ( "tr", [ Permutation.toUrlString settings.permutations.tr ] )
        , ( "bl", [ Permutation.toUrlString settings.permutations.bl ] )
        , ( "br", [ Permutation.toUrlString settings.permutations.br ] )
        , ( "v", [ "0" ] ) -- so I can potentially change the url format in the future
        ]


settingsFromQueryParams : QueryParameters -> Maybe UrlSettings
settingsFromQueryParams queryParameters =
    let
        getPermutation : String -> Maybe Permutation
        getPermutation key =
            Dict.get key queryParameters
                |> Maybe.andThen List.head
                |> Maybe.andThen Permutation.fromUrlString

        maybePermutations =
            Maybe.map4 (\tl tr bl br -> { tl = tl, tr = tr, bl = bl, br = br })
                (getPermutation "tl")
                (getPermutation "tr")
                (getPermutation "bl")
                (getPermutation "br")
    in
    Maybe.map3
        (\level colors permutations ->
            { level = level
            , initialVariables = colors
            , permutations = permutations
            }
        )
        (Dict.get "level" queryParameters |> Maybe.andThen List.head |> Maybe.andThen String.toInt)
        (Dict.get "colors" queryParameters
            |> Maybe.andThen List.head
            |> Maybe.map Colors.fromUrlString
        )
        maybePermutations
