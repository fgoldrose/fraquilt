module Messages exposing (..)

import DnDList
import Types exposing (Mode, Quadrant)


type Msg
    = NoOp
    | Randomize
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
    | StartSelection Quadrant Int
    | EndSelection Int
    | DnDMsg Quadrant DnDList.Msg
    | ToggleMode Mode
