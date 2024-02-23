port module Settings exposing (..)

import AppUrl exposing (AppUrl)
import Browser.Navigation as Nav
import Colors exposing (InitialVariables)
import Dict
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as Encode
import Messages exposing (Msg(..))
import Permutation exposing (Permutation)
import Random
import Svg
import Svg.Attributes as SvgAttr
import Types exposing (Quadrant(..), SelectionState(..))
import UI exposing (sectionWithName, sliderWithLabel)


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : InitialVariables
    , tl : Permutation
    , tr : Permutation
    , bl : Permutation
    , br : Permutation
    }


settingsEncoder : Settings -> Encode.Value
settingsEncoder settings =
    Encode.object
        [ ( "level", Encode.int settings.level )
        , ( "initialVariables", Colors.encode settings.initialVariables )
        , ( "permutations"
          , Encode.object
                [ ( "tl", Permutation.encode settings.tl )
                , ( "tr", Permutation.encode settings.tr )
                , ( "bl", Permutation.encode settings.bl )
                , ( "br", Permutation.encode settings.br )
                ]
          )
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
                , ( "tl", [ Permutation.toUrlString settings.tl ] )
                , ( "tr", [ Permutation.toUrlString settings.tr ] )
                , ( "bl", [ Permutation.toUrlString settings.bl ] )
                , ( "br", [ Permutation.toUrlString settings.br ] )
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
            , tl = permutations.tl
            , tr = permutations.tr
            , bl = permutations.bl
            , br = permutations.br
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
            , Colors.view UpdateInitialVar settings.initialVariables
                |> sectionWithName "Initial Colors"
            ]
        ]


helpIcon : Html Msg
helpIcon =
    Svg.svg
        [ SvgAttr.width "30px"
        , SvgAttr.height "30px"
        , SvgAttr.viewBox "0 0 15 15"
        , SvgAttr.fill "none"
        , HE.onClick (ShowHelpInfo True)
        , HA.style "cursor" "pointer"
        ]
        [ Svg.path
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.clipRule "evenodd"
            , SvgAttr.d "M0.877075 7.49972C0.877075 3.84204 3.84222 0.876892 7.49991 0.876892C11.1576 0.876892 14.1227 3.84204 14.1227 7.49972C14.1227 11.1574 11.1576 14.1226 7.49991 14.1226C3.84222 14.1226 0.877075 11.1574 0.877075 7.49972ZM7.49991 1.82689C4.36689 1.82689 1.82708 4.36671 1.82708 7.49972C1.82708 10.6327 4.36689 13.1726 7.49991 13.1726C10.6329 13.1726 13.1727 10.6327 13.1727 7.49972C13.1727 4.36671 10.6329 1.82689 7.49991 1.82689ZM8.24993 10.5C8.24993 10.9142 7.91414 11.25 7.49993 11.25C7.08571 11.25 6.74993 10.9142 6.74993 10.5C6.74993 10.0858 7.08571 9.75 7.49993 9.75C7.91414 9.75 8.24993 10.0858 8.24993 10.5ZM6.05003 6.25C6.05003 5.57211 6.63511 4.925 7.50003 4.925C8.36496 4.925 8.95003 5.57211 8.95003 6.25C8.95003 6.74118 8.68002 6.99212 8.21447 7.27494C8.16251 7.30651 8.10258 7.34131 8.03847 7.37854L8.03841 7.37858C7.85521 7.48497 7.63788 7.61119 7.47449 7.73849C7.23214 7.92732 6.95003 8.23198 6.95003 8.7C6.95004 9.00376 7.19628 9.25 7.50004 9.25C7.8024 9.25 8.04778 9.00601 8.05002 8.70417L8.05056 8.7033C8.05924 8.6896 8.08493 8.65735 8.15058 8.6062C8.25207 8.52712 8.36508 8.46163 8.51567 8.37436L8.51571 8.37433C8.59422 8.32883 8.68296 8.27741 8.78559 8.21506C9.32004 7.89038 10.05 7.35382 10.05 6.25C10.05 4.92789 8.93511 3.825 7.50003 3.825C6.06496 3.825 4.95003 4.92789 4.95003 6.25C4.95003 6.55376 5.19628 6.8 5.50003 6.8C5.80379 6.8 6.05003 6.55376 6.05003 6.25Z"
            , SvgAttr.fill "#000000"
            ]
            []
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
    let
        permutationConfig quadrant =
            { permutationSelection = Types.getSelectedForQuadrant quadrant selectionState
            , startSelection = StartSelection quadrant
            , endSelection = EndSelection
            , cancelSelection = CancelSelection
            }
    in
    [ Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "100px 100px"
        , HA.style "grid-gap" "30px"
        ]
        [ Permutation.view (permutationConfig TopLeft) settings.tl
        , Permutation.view (permutationConfig TopRight) settings.tr
        , Permutation.view (permutationConfig BottomLeft) settings.bl
        , Permutation.view (permutationConfig BottomRight) settings.br
        ]
    , Html.div
        [ HA.style "display" "flex"
        , HA.style "gap" "10px"
        , HA.style "align-items" "center"
        , HA.style "justify-items" "center"
        , HA.style "margin" "5px"
        ]
        [ Html.button [ HE.onClick Randomize ] [ Html.text "Random" ]
        , Html.button [ HE.onClick ClearPermutations ] [ Html.text "Clear" ]
        ]
    ]
        |> sectionWithName "Permutations"


randomPermutations : { initVars : InitialVariables, numVars : Int, level : Int } -> Random.Generator Settings
randomPermutations { initVars, numVars, level } =
    Random.map4
        (\tl tr bl br ->
            { level = level
            , initialVariables = initVars
            , tl = tl
            , tr = tr
            , bl = bl
            , br = br
            }
        )
        (Permutation.random numVars)
        (Permutation.random numVars)
        (Permutation.random numVars)
        (Permutation.random numVars)
