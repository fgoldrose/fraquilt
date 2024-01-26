module Messages exposing (..)

import DnDList
import Types exposing (Quadrant)


type Msg
    = NoOp
    | Randomize
    | RandomizePermutation
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
    | StartSelection Quadrant Int
    | EndSelection Int
    | DnDMsg Quadrant DnDList.Msg
