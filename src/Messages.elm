module Messages exposing (..)

import Browser
import Types exposing (Mode, Quadrant)
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
    | ToggleMode Mode
    | ShowHelpInfo Bool
