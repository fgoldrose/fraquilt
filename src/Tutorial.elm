module Tutorial exposing (..)

import Array
import Colors exposing (InitialVariables)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as HA
import Permutation exposing (Permutation)
import PermutationGrid exposing (PermutationGrid)
import Svg
import Svg.Attributes as SvgAttr
import Types exposing (PermutationSelection(..), Quadrant(..), SelectionState(..))


type Page
    = Page1 Page1State
    | Page2 Page2State
    | Page3 Page3State
    | Page4 Page3State
    | EndPage


initPage : String -> Maybe Page
initPage page =
    case page of
        "1" ->
            Just
                (Page1
                    { colors = Colors.init3
                    , hasSelectedColor = False
                    }
                )

        "2" ->
            Just
                (Page2
                    { colors = Colors.init3
                    , permutation = [ 1, 2, 0 ]
                    , selectedIndex = Nothing
                    , hasChangedPermutation = False
                    }
                )

        "3" ->
            Just
                (Page3
                    { colors = Colors.init3
                    , permutations =
                        { tl = [ 0, 1, 2 ]
                        , tr = [ 2, 0, 1 ]
                        , bl = [ 2, 1, 0 ]
                        , br = [ 1, 0, 2 ]
                        }
                    , selectionState = NoneSelected
                    , hasChangedPermutation = False
                    }
                )

        "4" ->
            Just
                (Page4
                    { colors = Colors.init3
                    , permutations =
                        { tl = [ 0, 1, 2 ]
                        , tr = [ 2, 0, 1 ]
                        , bl = [ 2, 1, 0 ]
                        , br = [ 1, 0, 2 ]
                        }
                    , selectionState = NoneSelected
                    , hasChangedPermutation = False
                    }
                )

        "end" ->
            Just EndPage

        _ ->
            Nothing


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
                Page1 state ->
                    page1 state

                Page2 state ->
                    page2 state

                Page3 state ->
                    page3 state

                Page4 state ->
                    page4 state

                EndPage ->
                    Html.div
                        [ HA.style "display" "flex"
                        , HA.style "flex-direction" "column"
                        , HA.style "align-items" "center"
                        , HA.style "gap" "20px"
                        , HA.style "padding" "20px"
                        ]
                        [ description "You have now seen how we will generate an image from an initial list of colors and 4 permutations."
                        , description "In this tutorial, the number of colors was always 3, and we only showed 2 levels of recursion."
                        , description "In the actual application, you can change the number of colors and the level of recursion."
                        , button
                            { href = "/fraquilt/"
                            , text = "Done"
                            }
                        ]
    in
    Html.div
        [ HA.style "font-family" "sans-serif"
        , HA.style "position" "relative"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100vh"
        , HA.style "width" "100vw"
        , HA.style "overflow" "scroll"
        , HA.id "page"
        ]
        [ Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "padding" "20px"
            , HA.style "border-bottom" "1px solid black"
            , HA.style "font-size" "30px"
            , HA.style "font-weight" "bold"
            ]
            [ Html.text "Tutorial" ]
        , Html.div
            [ HA.style "position" "absolute"
            , HA.style "top" "10px"
            , HA.style "right" "10px"
            ]
            [ xIcon
            ]
        , pageView
        ]


description : String -> Html Msg
description str =
    Html.span
        [ HA.style "text-align" "center"
        , HA.style "font-size" "20px"
        , HA.style "font-weight" "bold"
        ]
        [ Html.text str ]


descriptionLines : List String -> Html Msg
descriptionLines lines =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "10px"
        ]
        (List.map description lines)


button : { href : String, text : String } -> Html Msg
button { href, text } =
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
        [ Html.text text ]


nextButton : String -> Html Msg
nextButton href =
    button { href = href, text = "Next" }



-- Page 1: Colors


type alias Page1State =
    { colors : InitialVariables, hasSelectedColor : Bool }


page1 : Page1State -> Html Msg
page1 { colors, hasSelectedColor } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        , HA.style "padding" "20px"
        ]
        [ description "We start with a list of colors."
        , Colors.view ChangeInitialColor colors
        , description "Try changing the value of a color"
        , if hasSelectedColor then
            nextButton "/fraquilt/tutorial/2"

          else
            Html.text ""
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


smallRightArrow : Html msg
smallRightArrow =
    FeatherIcons.arrowRight
        |> FeatherIcons.withSize 20
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


page2 : Page2State -> Html Msg
page2 { colors, permutation, selectedIndex, hasChangedPermutation } =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        , HA.style "padding" "20px"
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



-- Page 3: Quadrants


grid :
    { tl : Html msg
    , tr : Html msg
    , bl : Html msg
    , br : Html msg
    }
    -> Int
    -> Html msg
grid items size =
    let
        gridItem i =
            Html.div
                [ HA.style "border" "0.5px solid black"
                , HA.style "display" "flex"
                , HA.style "width" (String.fromInt size ++ "px")
                , HA.style "height" (String.fromInt size ++ "px")
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                ]
                [ i ]
    in
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "1fr 1fr"
        , HA.style "border" "1px solid black"
        , HA.style "border-box" "box-sizing"
        ]
        [ gridItem items.tl, gridItem items.tr, gridItem items.bl, gridItem items.br ]


type alias Page3State =
    { colors : InitialVariables
    , permutations : PermutationGrid
    , selectionState : SelectionState
    , hasChangedPermutation : Bool
    }


permutationGridInputOutput : InitialVariables -> PermutationGrid -> (PermutationGrid -> Html Msg) -> Html Msg
permutationGridInputOutput colors permutations permutationsView =
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "row"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        , HA.style "max-width" "100%"
        ]
        [ Colors.view ChangeInitialColor colors
        , rightArrow
        , permutationsView
            permutations
        , rightArrow
        , grid
            { tl = Colors.readOnlyView (permute colors permutations.tl)
            , tr = Colors.readOnlyView (permute colors permutations.tr)
            , bl = Colors.readOnlyView (permute colors permutations.bl)
            , br = Colors.readOnlyView (permute colors permutations.br)
            }
            150
        ]


transform : String -> Html msg -> Html msg
transform scale item =
    Html.div
        [ HA.style "transform" ("scale(" ++ scale ++ ")")
        ]
        [ item ]


level2Grid : InitialVariables -> PermutationGrid -> Html msg
level2Grid colors permutations =
    let
        tlOutput =
            permute colors permutations.tl

        trOutput =
            permute colors permutations.tr

        blOutput =
            permute colors permutations.bl

        brOutput =
            permute colors permutations.br
    in
    grid
        { tl =
            grid
                { tl = Colors.readOnlyView (permute tlOutput permutations.tl)
                , tr = Colors.readOnlyView (permute tlOutput permutations.tr)
                , bl = Colors.readOnlyView (permute tlOutput permutations.bl)
                , br = Colors.readOnlyView (permute tlOutput permutations.br)
                }
                100
        , tr =
            grid
                { tl = Colors.readOnlyView (permute trOutput permutations.tl)
                , tr = Colors.readOnlyView (permute trOutput permutations.tr)
                , bl = Colors.readOnlyView (permute trOutput permutations.bl)
                , br = Colors.readOnlyView (permute trOutput permutations.br)
                }
                100
        , bl =
            grid
                { tl = Colors.readOnlyView (permute blOutput permutations.tl)
                , tr = Colors.readOnlyView (permute blOutput permutations.tr)
                , bl = Colors.readOnlyView (permute blOutput permutations.bl)
                , br = Colors.readOnlyView (permute blOutput permutations.br)
                }
                100
        , br =
            grid
                { tl = Colors.readOnlyView (permute brOutput permutations.tl)
                , tr = Colors.readOnlyView (permute brOutput permutations.tr)
                , bl = Colors.readOnlyView (permute brOutput permutations.bl)
                , br = Colors.readOnlyView (permute brOutput permutations.br)
                }
                100
        }
        200


page3 : Page3State -> Html Msg
page3 { colors, permutations, selectionState, hasChangedPermutation } =
    let
        subGridView c =
            Html.div
                [ HA.style "display" "flex"
                , HA.style "flex-direction" "row"
                , HA.style "align-items" "center"
                , HA.style "gap" "2px"
                ]
                [ Colors.readOnlyView c
                , smallRightArrow
                , Html.div
                    [ HA.style "width" "75px"
                    ]
                    [ Html.div
                        [ HA.style "transform" "scale(25%) translateX(-100%)"
                        ]
                        [ PermutationGrid.view
                            { selectionState = NoneSelected
                            , startSelection = \_ _ -> NoOp
                            , endSelection = \_ -> NoOp
                            , cancelSelection = NoOp
                            }
                            permutations
                        ]
                    ]
                , smallRightArrow
                , grid
                    { tl =
                        Colors.readOnlyView (permute c permutations.tl)
                            |> transform "25%"
                    , tr =
                        Colors.readOnlyView (permute c permutations.tr)
                            |> transform "25%"
                    , bl =
                        Colors.readOnlyView (permute c permutations.bl)
                            |> transform "25%"
                    , br =
                        Colors.readOnlyView (permute c permutations.br)
                            |> transform "25%"
                    }
                    40
                ]

        nextLevel =
            [ descriptionLines
                [ "The above transformation is level 1."
                , "For the next level, we will apply the same process again in each quadrant."
                , "The output list from the previous level becomes the input list for the next level."
                ]
            , grid
                { tl = subGridView (permute colors permutations.tl)
                , tr = subGridView (permute colors permutations.tr)
                , bl = subGridView (permute colors permutations.bl)
                , br = subGridView (permute colors permutations.br)
                }
                300
            , descriptionLines
                [ "We have divided the image into quadrants, and then divided each quadrant again."
                , "After 2 levels, we end up with these output color lists:"
                ]
            , level2Grid colors permutations
            , nextButton "/fraquilt/tutorial/4"
            ]
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        , HA.style "padding" "20px"
        ]
        ([ description "We will have 4 permutations, one for each quadrant."
         , description "For a given input list, we end up with an output list for each quadrant."
         , permutationGridInputOutput colors
            permutations
            (PermutationGrid.view
                { selectionState = selectionState
                , startSelection = StartSelectionQuadrant
                , endSelection = EndSelection
                , cancelSelection = CancelSelection
                }
            )
         ]
            ++ (if not hasChangedPermutation then
                    [ description "Try changing the permutations and see how the output changes" ]

                else
                    nextLevel
               )
        )


page4 : Page3State -> Html Msg
page4 { colors, permutations, selectionState } =
    let
        downArrow =
            FeatherIcons.arrowDown
                |> FeatherIcons.withSize 50
                |> FeatherIcons.toHtml
                    []

        outputImage =
            let
                tlOutput =
                    permute colors permutations.tl

                trOutput =
                    permute colors permutations.tr

                blOutput =
                    permute colors permutations.bl

                brOutput =
                    permute colors permutations.br

                colorDiv cs =
                    Html.div
                        [ HA.style "background-color" (Array.get 0 cs |> Maybe.withDefault "white")
                        , HA.style "width" "100px"
                        , HA.style "height" "100px"
                        ]
                        []

                gridColors { tl, tr, bl, br } =
                    Html.div
                        [ HA.style "display" "grid"
                        , HA.style "grid-template-columns" "1fr 1fr"
                        ]
                        [ colorDiv tl
                        , colorDiv tr
                        , colorDiv bl
                        , colorDiv br
                        ]

                level2 =
                    Html.div
                        [ HA.style "display" "grid"
                        , HA.style "grid-template-columns" "1fr 1fr"
                        , HA.style "border" "1px solid black"
                        ]
                        [ gridColors
                            { tl = permute tlOutput permutations.tl
                            , tr = permute tlOutput permutations.tr
                            , bl = permute tlOutput permutations.bl
                            , br = permute tlOutput permutations.br
                            }
                        , gridColors
                            { tl = permute trOutput permutations.tl
                            , tr = permute trOutput permutations.tr
                            , bl = permute trOutput permutations.bl
                            , br = permute trOutput permutations.br
                            }
                        , gridColors
                            { tl = permute blOutput permutations.tl
                            , tr = permute blOutput permutations.tr
                            , bl = permute blOutput permutations.bl
                            , br = permute blOutput permutations.br
                            }
                        , gridColors
                            { tl = permute brOutput permutations.tl
                            , tr = permute brOutput permutations.tr
                            , bl = permute brOutput permutations.bl
                            , br = permute brOutput permutations.br
                            }
                        ]
            in
            level2
    in
    Html.div
        [ HA.id "page"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "gap" "20px"
        , HA.style "padding" "20px"
        ]
        [ description "We can continue to apply the permutations recursively for however many levels we want."
        , description "But eventually, we want to transform the all these lists of colors into an image."
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "row"
            , HA.style "align-items" "center"
            , HA.style "gap" "20px"
            ]
            [ Colors.view ChangeInitialColor colors
            , rightArrow
            , PermutationGrid.view
                { selectionState = selectionState
                , startSelection = StartSelectionQuadrant
                , endSelection = EndSelection
                , cancelSelection = CancelSelection
                }
                permutations
            ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            ]
            [ Html.text "2 levels", downArrow ]
        , Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "row"
            , HA.style "align-items" "center"
            , HA.style "gap" "20px"
            , HA.style "flex-wrap" "wrap"
            ]
            [ level2Grid colors permutations
            , rightArrow
            , outputImage
            ]
        , description "We use the first color from a list as the value for that location in the image."
        , description "Try changing the initial colors and permutations to see how the output changes"
        , nextButton "/fraquilt/tutorial/end"
        ]



-- Update


type Msg
    = NoOp
    | ChangeInitialColor Int String
    | StartSelection Int
    | StartSelectionQuadrant Quadrant Int
    | EndSelection Int
    | CancelSelection


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case msg of
        NoOp ->
            ( page, Cmd.none )

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

                Page3 state ->
                    ( Page3
                        { state
                            | colors = Colors.set index color state.colors
                        }
                    , Cmd.none
                    )

                Page4 state ->
                    ( Page4
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

        StartSelectionQuadrant quadrant index ->
            let
                changeState state =
                    { state
                        | selectionState =
                            case quadrant of
                                TopLeft ->
                                    TLSelected index

                                TopRight ->
                                    TRSelected index

                                BottomLeft ->
                                    BLSelected index

                                BottomRight ->
                                    BRSelected index
                    }
            in
            case page of
                Page3 state ->
                    ( Page3 (changeState state)
                    , Cmd.none
                    )

                Page4 state ->
                    ( Page4 (changeState state)
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

                Page3 state ->
                    ( Page3
                        { state
                            | permutations =
                                PermutationGrid.endSelection state.selectionState index state.permutations
                            , selectionState = NoneSelected
                            , hasChangedPermutation = True
                        }
                    , Cmd.none
                    )

                Page4 state ->
                    ( Page4
                        { state
                            | permutations =
                                PermutationGrid.endSelection state.selectionState index state.permutations
                            , selectionState = NoneSelected
                        }
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

                Page3 state ->
                    ( Page3
                        { state
                            | selectionState = NoneSelected
                        }
                    , Cmd.none
                    )

                Page4 state ->
                    ( Page4
                        { state
                            | selectionState = NoneSelected
                        }
                    , Cmd.none
                    )

                _ ->
                    ( page, Cmd.none )
