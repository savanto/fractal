module Path exposing (Path(..), render)

import Coord exposing (Coord)


type Path
    = Move Coord
    | Line Coord
    | End


render : Path -> String
render path =
    case path of
        Move coord ->
            "M " ++ Coord.format coord

        Line coord ->
            "L " ++ Coord.format coord

        End ->
            "Z"
