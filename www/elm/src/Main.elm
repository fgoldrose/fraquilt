module Main exposing (..)

import Browser
import Html exposing (..)



--main : Program Flags Model Msg
--main =
--    Browser.element
--        { init = init
--        , view = view
--        , update = update
--        , subscriptions = subscriptions
--        }


type Operator
    = Add
    | Sub
    | Mult
    | Div


type Val
    = Var Int
    | Num Int


type MaybeExp
    = MaybeLeaf (Maybe Val)
    | MaybeNode (Maybe Operator) MaybeExp MaybeExp


type Expression
    = Leaf Val
    | Node Operator Expression Expression


type alias Picker =
    { numOps : Int
    , expression : MaybeExp
    }


type alias Model =
    { picker : Picker
    }


expSkeleton : Int -> MaybeExp
expSkeleton x =
    if x == 0 then
        MaybeLeaf Nothing

    else
        MaybeNode Nothing (expSkeleton (x - 1)) (expSkeleton (x - 1))



{-
   init : Flags -> ( Model, Cmd Msg )


   update : Msg -> Model -> ( Model, Cmd Msg )


   view : Model -> Html Msg


   subscriptions : Model -> Sub Msg
-}
