module Tutorial exposing (..)

import Array
import Colors exposing (InitialVariables)
import FeatherIcons
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Permutation exposing (Permutation)
import PermutationGrid exposing (PermutationGrid)
import Routing exposing (TutorialRoute(..))
import Svg.Attributes as SvgAttr
import Types exposing (PermutationSelection(..), Quadrant(..), SelectionState(..))
import UI exposing (pxFloat, pxInt)


type Page
    = Page1 Page1State
    | Page2 Page2State
    | Page3 Page3State
    | Page4 Page3State
    | EndPage


initPage : Routing.TutorialRoute -> Page
initPage page =
    case page of
        Tutorial1 ->
            Page1
                { colors = Colors.init3
                , hasSelectedColor = False
                }

        Tutorial2 ->
            Page2
                { colors = Colors.init3
                , permutation = [ 1, 2, 0 ]
                , selectedIndex = Nothing
                , hasChangedPermutation = False
                }

        Tutorial3 ->
            Page3
                { colors = Colors.init3
                , permutations =
                    { tl = [ 0, 1, 2 ]
                    , tr = [ 2, 0, 1 ]
                    , bl = [ 2, 1, 0 ]
                    , br = [ 1, 0, 2 ]
                    }
                , selectionState = NoneSelected
                , hasChangedPermutation = False
                , showProcess = True
                }

        Tutorial4 ->
            Page4
                { colors = Colors.init3
                , permutations =
                    { tl = [ 0, 1, 2 ]
                    , tr = [ 2, 0, 1 ]
                    , bl = [ 2, 1, 0 ]
                    , br = [ 1, 0, 2 ]
                    }
                , selectionState = NoneSelected
                , hasChangedPermutation = False
                , showProcess = True
                }

        TutorialEnd ->
            EndPage


xIcon : Html msg
xIcon =
    Html.a [ HA.href (Routing.reverse (Routing.App Nothing)) ]
        [ FeatherIcons.xCircle
            |> FeatherIcons.withSize 30
            |> FeatherIcons.toHtml [ SvgAttr.color "black" ]
        ]


view : Page -> Int -> Html Msg
view page windowWidth =
    let
        pageView =
            case page of
                Page1 state ->
                    page1 state

                Page2 state ->
                    page2 state

                Page3 state ->
                    page3 state windowWidth

                Page4 state ->
                    page4 state windowWidth

                EndPage ->
                    Html.div
                        [ HA.class "flex flex-col items-center gap-5 p-5" ]
                        [ description "You have now seen how we will generate an image from an initial list of colors and 4 permutations."
                        , description "In this tutorial, the number of colors was always 3, and we only showed up to 2 levels of recursion."
                        , description "In the actual application, you can change the number of colors and the level of recursion."
                        , description "You will also be able to randomize the permutations. There is a 'Random' button that will generate all 4 permutations randomly, and a 'Random Symmetric' button that will generate 2 random permutation and have equal diagonal permutations (because symmetry is pretty)."
                        , description "Clicking on the image does the same thing as clicking the 'Random Symmetric' button."
                        , button
                            { route = Routing.App Nothing
                            , text = "Done"
                            }
                        ]
    in
    Html.div
        [ HA.class "font-sans relative flex flex-col h-screen w-screen overflow-scroll"
        , HA.id "page"
        ]
        [ Html.div
            [ HA.class "flex flex-col items-center p-5 border-b border-b-black text-3xl font-bold"
            ]
            [ Html.text "Tutorial" ]
        , Html.div
            [ HA.class "absolute top-3 right-3"
            ]
            [ xIcon
            ]
        , pageView
        ]


description : String -> Html Msg
description str =
    Html.span
        [ HA.class "text-center text-xl font-bold"
        ]
        [ Html.text str ]


descriptionLines : List String -> Html Msg
descriptionLines lines =
    Html.div
        [ HA.class "flex flex-col items-center gap-3"
        ]
        (List.map description lines)


button : { route : Routing.Route, text : String } -> Html Msg
button { route, text } =
    Html.a
        [ HA.href (Routing.reverse route)
        , HA.class "bg-green-600 hover:bg-green-700 p-2 text-center rounded text-white font-bold cursor-pointer no-underline"
        ]
        [ Html.text text ]


nextButton : Routing.TutorialRoute -> Html Msg
nextButton tutorialRoute =
    button { route = Routing.Tutorial tutorialRoute, text = "Next" }



-- Page 1: Colors


type alias Page1State =
    { colors : InitialVariables, hasSelectedColor : Bool }


page1 : Page1State -> Html Msg
page1 { colors, hasSelectedColor } =
    Html.div
        [ HA.class "flex flex-col items-center gap-5 p-5" ]
        [ description "We start with a list of colors."
        , Colors.view 30 (Just ChangeInitialColor) colors
        , description "Try changing the value of a color"
        , if hasSelectedColor then
            nextButton Routing.Tutorial2

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


page2 : Page2State -> Html Msg
page2 { colors, permutation, selectedIndex, hasChangedPermutation } =
    Html.div
        [ HA.class "flex flex-col items-center gap-5 p-5" ]
        ([ description "A permutation changes the order of a list of colors"
         , Html.div
            [ HA.class "flex flex-wrap items-center justify-center gap-4" ]
            [ Colors.view 30 (Just ChangeInitialColor) colors
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
                , dotPixelSize = 20
                }
                permutation
            , rightArrow
            , Colors.view 30 Nothing (permute colors permutation)
            ]
         ]
            ++ (if hasChangedPermutation then
                    [ description "See how the output changes depending on the permutation"
                    , nextButton Routing.Tutorial3
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
    -> Float
    -> Html msg
grid items size =
    let
        gridItem i =
            Html.div
                [ HA.class "border-[0.5px] border-black flex items-center justify-center"
                , HA.style "width" (String.fromFloat (size / 2) ++ "px")
                , HA.style "height" (String.fromFloat (size / 2) ++ "px")
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
    , showProcess : Bool
    }


level2Grid : Float -> InitialVariables -> PermutationGrid -> Html msg
level2Grid size colors permutations =
    let
        tlOutput =
            permute colors permutations.tl

        trOutput =
            permute colors permutations.tr

        blOutput =
            permute colors permutations.bl

        brOutput =
            permute colors permutations.br

        smallerSize =
            size / 2

        smallerColorView c =
            Colors.view (size / 16) Nothing c
    in
    grid
        { tl =
            grid
                { tl = smallerColorView (permute tlOutput permutations.tl)
                , tr = smallerColorView (permute tlOutput permutations.tr)
                , bl = smallerColorView (permute tlOutput permutations.bl)
                , br = smallerColorView (permute tlOutput permutations.br)
                }
                smallerSize
        , tr =
            grid
                { tl = smallerColorView (permute trOutput permutations.tl)
                , tr = smallerColorView (permute trOutput permutations.tr)
                , bl = smallerColorView (permute trOutput permutations.bl)
                , br = smallerColorView (permute trOutput permutations.br)
                }
                smallerSize
        , bl =
            grid
                { tl = smallerColorView (permute blOutput permutations.tl)
                , tr = smallerColorView (permute blOutput permutations.tr)
                , bl = smallerColorView (permute blOutput permutations.bl)
                , br = smallerColorView (permute blOutput permutations.br)
                }
                smallerSize
        , br =
            grid
                { tl = smallerColorView (permute brOutput permutations.tl)
                , tr = smallerColorView (permute brOutput permutations.tr)
                , bl = smallerColorView (permute brOutput permutations.bl)
                , br = smallerColorView (permute brOutput permutations.br)
                }
                smallerSize
        }
        size


page3 : Page3State -> Int -> Html Msg
page3 { colors, permutations, selectionState, hasChangedPermutation, showProcess } windowWidth =
    let
        permutationGridInputOutput =
            Html.div
                [ HA.class "flex items-center gap-5 max-w-full flex-wrap justify-center"
                ]
                [ Colors.view 30 (Just ChangeInitialColor) colors
                , rightArrow
                , PermutationGrid.view
                    { selectionState = selectionState
                    , startSelection = StartSelectionQuadrant
                    , endSelection = EndSelection
                    , cancelSelection = CancelSelection
                    , dotPixelSize = 20
                    }
                    permutations
                , rightArrow
                , grid
                    { tl = Colors.view 30 Nothing (permute colors permutations.tl)
                    , tr = Colors.view 30 Nothing (permute colors permutations.tr)
                    , bl = Colors.view 30 Nothing (permute colors permutations.bl)
                    , br = Colors.view 30 Nothing (permute colors permutations.br)
                    }
                    (min (0.9 * toFloat windowWidth) 300)
                ]

        topLevelGridSize =
            min (0.9 * toFloat windowWidth) 600

        subGridView c =
            Html.div
                [ HA.class "flex items-center justify-center gap-0.5 flex-wrap"
                ]
                [ Colors.view 20 Nothing c
                , smallRightArrow
                , PermutationGrid.view
                    { selectionState = NoneSelected
                    , startSelection = \_ _ -> NoOp
                    , endSelection = \_ -> NoOp
                    , cancelSelection = NoOp
                    , dotPixelSize = 5
                    }
                    permutations
                , smallRightArrow
                , let
                    smallGridSize =
                        topLevelGridSize / 4

                    colorSize =
                        smallGridSize / 9
                  in
                  grid
                    { tl = Colors.view colorSize Nothing (permute c permutations.tl)
                    , tr = Colors.view colorSize Nothing (permute c permutations.tr)
                    , bl = Colors.view colorSize Nothing (permute c permutations.bl)
                    , br = Colors.view colorSize Nothing (permute c permutations.br)
                    }
                    smallGridSize
                ]

        nextLevel =
            [ descriptionLines
                [ "We then repeat this process in each quadrant."
                , "The output list in each quadrant becomes the input list for the next level."
                ]
            , if showProcess then
                grid
                    { tl = subGridView (permute colors permutations.tl)
                    , tr = subGridView (permute colors permutations.tr)
                    , bl = subGridView (permute colors permutations.bl)
                    , br = subGridView (permute colors permutations.br)
                    }
                    topLevelGridSize

              else
                level2Grid topLevelGridSize colors permutations
            , Html.button
                [ HE.onClick (ToggleShowProcess (not showProcess))
                , HA.class "p-2 text-center rounded-md text-black border border-black font-bold no-underline cursor-pointer hover:bg-neutral-50"
                ]
                [ if showProcess then
                    Html.text "Show output"

                  else
                    Html.text "Show process"
                ]
            , description
                "For each additional level of recursion, we will apply the permutations again to each output from the previous level, subdividing each square into 4 smaller quadrants."
            , nextButton Routing.Tutorial4
            ]
    in
    Html.div
        [ HA.class "flex flex-col items-center gap-5 p-5" ]
        ([ description "We will have 4 permutations, one for each quadrant."
         , description "After applying these permutations to the input list of colors, we end up with an output list for each quadrant."
         , permutationGridInputOutput
         ]
            ++ (if not hasChangedPermutation then
                    [ description "Try changing the permutations and see how the output changes" ]

                else
                    nextLevel
               )
        )


page4 : Page3State -> Int -> Html Msg
page4 { colors, permutations, selectionState } windowWidth =
    let
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
                        , HA.style "width" (pxInt (min (windowWidth // 5) 100))
                        , HA.style "height" (pxInt (min (windowWidth // 5) 100))
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
            in
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
    Html.div
        [ HA.id "page"
        , HA.class "flex flex-col items-center gap-5 p-5"
        ]
        [ description "We can continue to apply the permutations recursively, and we end up with a grid of color lists."
        , Html.div
            [ HA.class "flex items-center gap-5 flex-wrap justify-center" ]
            [ Colors.view 30 (Just ChangeInitialColor) colors
            , rightArrow
            , PermutationGrid.view
                { selectionState = selectionState
                , startSelection = StartSelectionQuadrant
                , endSelection = EndSelection
                , cancelSelection = CancelSelection
                , dotPixelSize = 20
                }
                permutations
            , Html.div
                [ HA.class "flex flex-col items-center" ]
                [ Html.text "1 level", rightArrow ]
            , grid
                { tl = Colors.view 30 Nothing (permute colors permutations.tl)
                , tr = Colors.view 30 Nothing (permute colors permutations.tr)
                , bl = Colors.view 30 Nothing (permute colors permutations.bl)
                , br = Colors.view 30 Nothing (permute colors permutations.br)
                }
                300
            , Html.div
                [ HA.class "flex flex-col items-center justify-center" ]
                [ Html.text "2 levels", rightArrow ]
            , level2Grid 300 colors permutations
            ]
        , description "But after some number of levels, we will want to transform the grid of color lists into an image."
        , description "We use the first color from a list as the value for that location in the image."
        , Html.div
            [ HA.class "flex items-center gap-5 flex-wrap justify-center" ]
            [ level2Grid (min (0.9 * toFloat windowWidth) 400) colors permutations
            , rightArrow
            , outputImage
            ]
        , description "Try changing the initial colors and permutations to see how the output changes"
        , nextButton Routing.TutorialEnd
        ]



-- Update


type Msg
    = NoOp
    | ChangeInitialColor Int String
    | StartSelection Int
    | StartSelectionQuadrant Quadrant Int
    | EndSelection Int
    | CancelSelection
    | ToggleShowProcess Bool


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

        ToggleShowProcess showProcess ->
            case page of
                Page3 state ->
                    ( Page3
                        { state
                            | showProcess = showProcess
                        }
                    , Cmd.none
                    )

                _ ->
                    ( page, Cmd.none )
