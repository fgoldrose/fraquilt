module Messages exposing (..)

import Types exposing (Quadrant)


type Msg
    = NoOp
    | Randomize
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
    | StartSelection Quadrant Int
    | EndSelection Int
