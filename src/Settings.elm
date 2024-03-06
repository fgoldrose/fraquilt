port module Settings exposing (..)

import AppUrl exposing (AppUrl)
import Browser.Navigation as Nav
import Colors exposing (InitialVariables)
import Dict
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Messages exposing (Msg(..))
import Permutation exposing (Permutation)
import PermutationGrid exposing (PermutationGrid)
import Random
import Svg.Attributes as SvgAttr
import Types exposing (Quadrant(..), SelectionState(..))
import UI exposing (sectionWithName, sliderWithLabel)


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : InitialVariables
    , permutations : PermutationGrid
    }


settingsEncoder : Settings -> Encode.Value
settingsEncoder settings =
    Encode.object
        [ ( "level", Encode.int settings.level )
        , ( "initialVariables", Colors.encode settings.initialVariables )
        , ( "permutations", PermutationGrid.encode settings.permutations )
        ]


change : Nav.Key -> Settings -> Cmd msg
change key settings =
    Cmd.batch
        [ render settings
        , setUrl key settings
        ]


render : Settings -> Cmd msg
render settings =
    renderImage (settingsEncoder settings)


setUrl : Nav.Key -> Settings -> Cmd msg
setUrl key settings =
    let
        queryParameters =
            Dict.fromList
                [ ( "level", [ String.fromInt settings.level ] )
                , ( "colors", [ Colors.toUrlString settings.initialVariables ] )
                , ( "tl", [ Permutation.toUrlString settings.permutations.tl ] )
                , ( "tr", [ Permutation.toUrlString settings.permutations.tr ] )
                , ( "bl", [ Permutation.toUrlString settings.permutations.bl ] )
                , ( "br", [ Permutation.toUrlString settings.permutations.br ] )
                , ( "v", [ "0" ] ) -- so I can potentially change the url format in the future
                ]

        appUrl =
            { path = [ "fraquilt", "" ]
            , fragment = Nothing
            , queryParameters = queryParameters
            }
    in
    Nav.pushUrl key (AppUrl.toString appUrl)


fromUrl : AppUrl -> Maybe Settings
fromUrl appUrl =
    let
        getPermutation : String -> Maybe Permutation
        getPermutation key =
            Dict.get key appUrl.queryParameters
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
        (Dict.get "level" appUrl.queryParameters |> Maybe.andThen List.head |> Maybe.andThen String.toInt)
        (Dict.get "colors" appUrl.queryParameters
            |> Maybe.andThen List.head
            |> Maybe.map Colors.fromUrlString
        )
        maybePermutations


viewEditSettings : SelectionState -> Settings -> Html Msg
viewEditSettings selectionState settings =
    Html.div
        [ HA.style "height" "100%"
        , HA.style "max-height" "100vh"
        , HA.style "flex-grow" "2"
        , HA.style "overflow-y" "scroll"
        , HA.style "background-color" "rgb(240, 240, 240)"
        , HA.style "position" "relative"
        ]
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "10px"
            , HA.style "right" "10px"
            ]
            [ helpIcon
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "gap" "20px"
            , HA.style "margin" "50px 10px"
            ]
            [ Html.div
                [ HA.style "font-size" "20px"
                , HA.style "font-weight" "bold"
                ]
                [ Html.text "Configuration" ]
            , viewLevel settings.level
            , viewPermutationGrid selectionState settings
            , viewNumberOfVariables (Colors.count settings.initialVariables)
            , [ Colors.view UpdateInitialVar settings.initialVariables ]
                |> sectionWithName "Initial Colors"
            ]
        ]


helpIcon : Html Msg
helpIcon =
    Html.a [ HA.href "/fraquilt/tutorial/1" ]
        [ FeatherIcons.helpCircle
            |> FeatherIcons.withSize 30
            |> FeatherIcons.toHtml [ SvgAttr.color "black" ]
        ]


viewLevel : Int -> Html Msg
viewLevel level =
    [ sliderWithLabel
        { id = "level"
        , value = level
        , msg = ChangeLevel
        , min = 1
        , max = 10
        }
    ]
        |> sectionWithName "Level"


viewNumberOfVariables : Int -> Html Msg
viewNumberOfVariables numVars =
    [ Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "gap" "10px"
        ]
        [ Html.button [ HE.onClick (ChangeNumberOfVariables (numVars - 1)) ]
            [ Html.text "-" ]
        , Html.span [] [ Html.text (String.fromInt numVars) ]
        , Html.button [ HE.onClick (ChangeNumberOfVariables (numVars + 1)) ]
            [ Html.text "+" ]
        ]
    ]
        |> sectionWithName "Number of colors"


viewPermutationGrid : SelectionState -> Settings -> Html Msg
viewPermutationGrid selectionState settings =
    [ PermutationGrid.view
        { startSelection = StartSelection
        , endSelection = EndSelection
        , cancelSelection = CancelSelection
        , selectionState = selectionState
        }
        settings.permutations
    , Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "10px"
        , HA.style "align-items" "center"
        , HA.style "justify-items" "center"
        , HA.style "margin" "5px"
        ]
        [ Html.button [ HE.onClick <| Randomize { symmetric = False } ] [ Html.text "Random" ]
        , Html.button [ HE.onClick <| Randomize { symmetric = True } ] [ Html.text "Random Symmetric" ]
        , Html.button [ HE.onClick ClearPermutations ] [ Html.text "Clear" ]
        ]
    ]
        |> sectionWithName "Permutations"
