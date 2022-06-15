module Fill exposing (Fill(..), render)


type Fill
    = Black
    | White


render : Fill -> String
render fill =
    case fill of
        Black ->
            "black"

        White ->
            "white"
