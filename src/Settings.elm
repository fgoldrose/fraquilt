port module Settings exposing (..)

import Json.Encode as Encode
import Random


port renderImage : Encode.Value -> Cmd msg


type alias Settings =
    { level : Int
    , initialVariables : List Int
    , tl : List Int
    , tr : List Int
    , bl : List Int
    , br : List Int
    }


render : Settings -> Cmd msg
render settings =
    renderImage <|
        Encode.object
            [ ( "level", Encode.int settings.level )
            , ( "initialVariables", Encode.list Encode.int settings.initialVariables )
            , ( "colorAdjustments"
              , Encode.object
                    [ ( "tl", Encode.list Encode.int settings.tl )
                    , ( "tr", Encode.list Encode.int settings.tr )
                    , ( "bl", Encode.list Encode.int settings.bl )
                    , ( "br", Encode.list Encode.int settings.br )
                    ]
              )
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
        (Random.list numVars (Random.int 0 (numVars - 1)))
        (Random.list numVars (Random.int 0 (numVars - 1)))
        (Random.list numVars (Random.int 0 (numVars - 1)))
        (Random.list numVars (Random.int 0 (numVars - 1)))
