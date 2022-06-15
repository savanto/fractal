module Matrix exposing (Matrix, transform)

import Coord exposing (Coord)


type alias Matrix =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    , f : Float
    }


transform : Matrix -> Coord -> Coord
transform { a, b, c, d, e, f } { x, y } =
    { x = a * x + c * y + e
    , y = b * x + d * y + f
    }
