module Messages exposing (..)

import Browser
import Types exposing (Quadrant)
import Url exposing (Url)


type Msg
    = NoOp
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | Randomize
    | ClearPermutations
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
    | StartSelection Quadrant Int
    | EndSelection Int
    | CancelSelection
    | ShowHelpInfo Bool
