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
import Routing
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
        , Nav.pushUrl key (Routing.reverse (Routing.App (Just settings)))
        ]


render : Settings -> Cmd msg
render settings =
    renderImage (settingsEncoder settings)


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
    Html.a [ HA.href (Routing.reverse <| Routing.Tutorial Routing.Tutorial1) ]
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
        , dotPixelSize = 20
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
