port module Settings exposing (..)

import ColorAdjustments exposing (ColorAdjustments)
import Html exposing (Html)
import Html.Attributes as HA
import Json.Encode as Encode
import Random


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : List Int
    , tl : ColorAdjustments
    , tr : ColorAdjustments
    , bl : ColorAdjustments
    , br : ColorAdjustments
    }


render : Settings -> Cmd msg
render settings =
    renderImage <|
        Encode.object
            [ ( "level", Encode.int settings.level )
            , ( "initialVariables", Encode.list Encode.int settings.initialVariables )
            , ( "colorAdjustments"
              , Encode.object
                    [ ( "tl", ColorAdjustments.encode settings.tl )
                    , ( "tr", ColorAdjustments.encode settings.tr )
                    , ( "bl", ColorAdjustments.encode settings.bl )
                    , ( "br", ColorAdjustments.encode settings.br )
                    ]
              )
            ]


viewEditSettings : Settings -> Html msg
viewEditSettings settings =
    Html.div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "1fr 1fr"
        , HA.style "grid-gap" "50px"
        ]
        [ ColorAdjustments.view settings.tl
        , ColorAdjustments.view settings.tr
        , ColorAdjustments.view settings.bl
        , ColorAdjustments.view settings.br
        ]


random : { numVars : Int, level : Int } -> Random.Generator Settings
random { numVars, level } =
    Random.map5
        (\initialVariables tl tr bl br ->
            { level = level
            , initialVariables = initialVariables
            , tl = tl
            , tr = tr
            , bl = bl
            , br = br
            }
        )
        (Random.list numVars (Random.int 0 255))
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
        (ColorAdjustments.random numVars)
