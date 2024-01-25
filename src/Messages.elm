module Messages exposing (..)


type Msg
    = NoOp
    | Randomize
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
