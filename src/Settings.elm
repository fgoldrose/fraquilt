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
        [ HA.class "h-full grow-[2] overflow-y-scroll bg-neutral-100 relative tall:w-full" ]
        [ Html.div
            [ HA.class "absolute top-3 right-3" ]
            [ helpIcon ]
        , Html.div
            [ HA.class "flex flex-col items-center gap-5 my-12 mx-3" ]
            [ Html.div
                [ HA.class "text-2xl font-bold" ]
                [ Html.text "Fraquilt" ]
            , viewLevel settings.level
            , viewPermutationGrid selectionState settings
            , viewNumberOfVariables (Colors.count settings.initialVariables)
            , [ Colors.view 30 (Just UpdateInitialVar) settings.initialVariables ]
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
        [ HA.class "flex items-center gap-2"
        ]
        [ Html.button
            [ HA.class "border border-black rounded-md px-2 hover:bg-neutral-200"
            , HE.onClick (ChangeNumberOfVariables (numVars - 1))
            ]
            [ Html.text "-" ]
        , Html.span [] [ Html.text (String.fromInt numVars) ]
        , Html.button
            [ HA.class "border border-black rounded-md px-2 hover:bg-neutral-200"
            , HE.onClick (ChangeNumberOfVariables (numVars + 1))
            ]
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
        [ HA.class "flex gap-2 items-center justify-items-center m-1 text-sm"
        ]
        [ Html.button
            [ HA.class "border border-black rounded-md px-2 hover:bg-neutral-200"
            , HE.onClick <| Randomize { symmetric = False }
            ]
            [ Html.text "Random" ]
        , Html.button
            [ HA.class "border border-black rounded-md px-2 hover:bg-neutral-200"
            , HE.onClick <| Randomize { symmetric = True }
            ]
            [ Html.text "Random Symmetric" ]
        , Html.button
            [ HA.class "border border-black rounded-md px-2 hover:bg-neutral-200"
            , HE.onClick ClearPermutations
            ]
            [ Html.text "Clear" ]
        ]
    ]
        |> sectionWithName "Permutations"
