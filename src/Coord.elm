module Coord exposing (Coord, format)


type alias Coord =
    { x : Float
    , y : Float
    }


format : Coord -> String
format { x, y } =
    [ x, y ]
        |> List.map String.fromFloat
        |> String.join ","
