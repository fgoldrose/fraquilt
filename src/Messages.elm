module Messages exposing (..)

import Types exposing (Mode, Quadrant)


type Msg
    = NoOp
    | Randomize
    | ClearPermutations
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
    | StartSelection Quadrant Int
    | EndSelection Int
    | CancelSelection
    | ToggleMode Mode
    | ShowHelpInfo Bool
