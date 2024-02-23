module Types exposing (..)


type Quadrant
    = TopLeft
    | TopRight
    | BottomLeft
    | BottomRight


type SelectionState
    = NoneSelected
    | TLSelected Int
    | TRSelected Int
    | BLSelected Int
    | BRSelected Int


type PermutationSelection
    = DontPromptSelection
    | PromptSelection
    | Selected Int


getSelectedForQuadrant : Quadrant -> SelectionState -> PermutationSelection
getSelectedForQuadrant quadrant selectionState =
    case ( quadrant, selectionState ) of
        ( TopLeft, TLSelected x ) ->
            Selected x

        ( TopRight, TRSelected x ) ->
            Selected x

        ( BottomLeft, BLSelected x ) ->
            Selected x

        ( BottomRight, BRSelected x ) ->
            Selected x

        ( _, NoneSelected ) ->
            PromptSelection

        _ ->
            DontPromptSelection
