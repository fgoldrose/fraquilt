module DragAndDrop exposing (..)

import DnDList
import Messages exposing (Msg(..))
import Types exposing (Quadrant(..))


config : DnDList.Config Int
config =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrop
    , operation = DnDList.Swap
    }


getSystemForQuadrant : Quadrant -> DnDList.System Int Msg
getSystemForQuadrant quadrant =
    case quadrant of
        TopLeft ->
            tlSystem

        TopRight ->
            trSystem

        BottomLeft ->
            blSystem

        BottomRight ->
            brSystem


tlSystem : DnDList.System Int Msg
tlSystem =
    DnDList.create config (DnDMsg TopLeft)


trSystem : DnDList.System Int Msg
trSystem =
    DnDList.create config (DnDMsg TopRight)


blSystem : DnDList.System Int Msg
blSystem =
    DnDList.create config (DnDMsg BottomLeft)


brSystem : DnDList.System Int Msg
brSystem =
    DnDList.create config (DnDMsg BottomRight)
