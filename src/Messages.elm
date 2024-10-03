module Messages exposing (..)

import Browser
import Tutorial
import Types exposing (Quadrant)
import Url exposing (Url)


type Msg
    = NoOp
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | Randomize { symmetric : Bool }
    | ClearPermutations
    | UpdateInitialVar Int String
    | ChangeLevel String
    | ChangeNumberOfVariables Int
    | StartSelection Quadrant Int
    | EndSelection Int
    | CancelSelection
    | TutorialMsg Tutorial.Msg
    | WindowChanged Int Int
