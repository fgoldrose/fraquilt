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


getSelectedForQuadrant : Quadrant -> SelectionState -> Maybe Int
getSelectedForQuadrant quadrant selectionState =
    case ( quadrant, selectionState ) of
        ( TopLeft, TLSelected x ) ->
            Just x

        ( TopRight, TRSelected x ) ->
            Just x

        ( BottomLeft, BLSelected x ) ->
            Just x

        ( BottomRight, BRSelected x ) ->
            Just x

        _ ->
            Nothing
