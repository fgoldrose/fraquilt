module Tutorial exposing (..)

import Array
import Colors exposing (InitialVariables)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as HA
import Permutation exposing (Permutation)
import Svg
import Svg.Attributes as SvgAttr
import Types exposing (PermutationSelection(..))


type Page
    = Intro
    | Page1 Page1State
    | Page2 Page2State


initPage : String -> Page
initPage page =
    case page of
        "1" ->
            Page1
                { colors = Colors.init3
                , hasSelectedColor = False
                }

        "2" ->
            Page2
                { colors = Colors.init3
                , permutation = [ 1, 2, 0 ]
                , selectedIndex = Nothing
                , hasChangedPermutation = False
                }

        _ ->
            Intro


xIcon : Html msg
xIcon =
    Html.a [ HA.href "/fraquilt/" ]
        [ FeatherIcons.xCircle
            |> FeatherIcons.withSize 30
            |> FeatherIcons.toHtml [ SvgAttr.color "black" ]
        ]


view : Page -> Html Msg
view page =
    let
        pageView =
            case page of
                Intro ->
                    [ description "Welcome to the tutorial."
                    , nextButton "/fraquilt/tutorial/1"
                    ]

                Page1 state ->
                    page1 state

                Page2 state ->
                    page2 state
    in
    Html.div
        [ HA.style "font-family" "sans-serif"
        , HA.style "position" "relative"
        ]
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "10px"
            , HA.style "right" "10px"
            ]
            [ xIcon
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "height" "100vh"
            , HA.style "gap" "5px"
            ]
            pageView
        ]


description : String -> Html Msg
description str =
    Html.h3
        []
        [ Html.text str ]


nextButton : String -> Html Msg
nextButton href =
    Html.a
        [ HA.href href
        , HA.style "background" "green"
        , HA.style "padding" "10px"
        , HA.style "text-align" "center"
        , HA.style "border-radius" "5px"
        , HA.style "color" "white"
        , HA.style "font-weight" "bold"
        , HA.style "text-decoration" "none"
        , HA.style "cursor" "pointer"
        ]
        [ Html.text "Next" ]



-- Page 1: Colors


type alias Page1State =
    { colors : InitialVariables, hasSelectedColor : Bool }


page1 : Page1State -> List (Html Msg)
page1 { colors, hasSelectedColor } =
    [ Html.div
        [ HA.style "height" "300px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        ]
        [ description "We start with a list of colors."
        , Colors.view ChangeInitialColor colors
        , description "Try changing the value of a color"
        , if hasSelectedColor then
            nextButton "/fraquilt/tutorial/2"

          else
            Html.text ""
        ]
    ]



-- Page 2: Permutation


type alias Page2State =
    { colors : InitialVariables
    , permutation : Permutation
    , selectedIndex : Maybe Int
    , hasChangedPermutation : Bool
    }


rightArrow : Html msg
rightArrow =
    FeatherIcons.arrowRight
        |> FeatherIcons.withSize 50
        |> FeatherIcons.toHtml []


permute : InitialVariables -> Permutation -> InitialVariables
permute colors permutation =
    List.filterMap (\index -> Array.get index colors) permutation
        |> Array.fromList


permutationInputOutput : InitialVariables -> Maybe Int -> Permutation -> Html Msg
permutationInputOutput colors selectedIndex permutation =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        ]
        [ Colors.view ChangeInitialColor colors
        , rightArrow
        , Permutation.view
            { permutationSelection =
                case selectedIndex of
                    Just index ->
                        Selected index

                    Nothing ->
                        PromptSelection
            , startSelection = StartSelection
            , endSelection = EndSelection
            , cancelSelection = CancelSelection
            }
            permutation
        , rightArrow
        , Colors.readOnlyView (permute colors permutation)
        ]


page2 : Page2State -> List (Html Msg)
page2 { colors, permutation, selectedIndex, hasChangedPermutation } =
    [ Html.div
        [ HA.style "height" "300px"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        ]
        ([ description "A permutation changes the order of a list of colors"
         , permutationInputOutput colors selectedIndex permutation
         ]
            ++ (if hasChangedPermutation then
                    [ description "See how the output changes depending on the permutation"
                    , nextButton "/fraquilt/tutorial/3"
                    ]

                else
                    case selectedIndex of
                        Nothing ->
                            [ description "Try clicking one of the blue dots on the permutation" ]

                        Just _ ->
                            [ description "Now click on another blue dot to swap those positions in the permutation" ]
               )
        )
    ]



-- Update


type Msg
    = ChangeInitialColor Int String
    | StartSelection Int
    | EndSelection Int
    | CancelSelection


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case msg of
        ChangeInitialColor index color ->
            case page of
                Page1 state ->
                    ( Page1
                        { state
                            | colors = Colors.set index color state.colors
                            , hasSelectedColor = True
                        }
                    , Cmd.none
                    )

                Page2 state ->
                    ( Page2
                        { state
                            | colors = Colors.set index color state.colors
                        }
                    , Cmd.none
                    )

                _ ->
                    ( page, Cmd.none )

        StartSelection index ->
            case page of
                Page2 state ->
                    ( Page2
                        { state
                            | selectedIndex = Just index
                        }
                    , Cmd.none
                    )

                _ ->
                    ( page, Cmd.none )

        EndSelection index ->
            case page of
                Page2 state ->
                    ( Page2
                        (case state.selectedIndex of
                            Just selectedIndex ->
                                { state
                                    | permutation =
                                        Permutation.swap selectedIndex index state.permutation
                                    , selectedIndex = Nothing
                                    , hasChangedPermutation = True
                                }

                            Nothing ->
                                state
                        )
                    , Cmd.none
                    )

                _ ->
                    ( page, Cmd.none )

        CancelSelection ->
            case page of
                Page2 state ->
                    ( Page2
                        { state
                            | selectedIndex = Nothing
                        }
                    , Cmd.none
                    )

                _ ->
                    ( page, Cmd.none )
