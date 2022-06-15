module Block exposing (Block, area, next, render)

import Coord exposing (Coord)
import Fill exposing (Fill(..))
import Matrix
import Path exposing (Path(..))
import Svg
import Svg.Attributes as SvgAttrs


type alias Block =
    { topLeft : Coord
    , topRight : Coord
    , botRight : Coord
    , botLeft : Coord
    , fill : Fill
    }


next : Block -> Block
next prev =
    let
        -- Rotation
        cx =
            prev.topLeft.x

        cy =
            prev.topLeft.y

        -- Scaling
        sx =
            sqrt 2 / 2

        sy =
            sqrt 2 / 2

        -- Translation
        tx =
            prev.topRight.x - prev.topLeft.x * sx

        ty =
            prev.topRight.y - prev.topLeft.y * sy

        -- 45°
        a =
            pi / 4

        matrix =
            { a = sx * cos a
            , b = sy * sin a
            , c = -sx * sin a
            , d = sy * cos a
            , e = (-cx * cos a + cy * sin a + cx) * sx + tx
            , f = (-cx * sin a - cy * cos a + cy) * sy + ty
            }

        fill =
            case prev.fill of
                Black ->
                    White

                White ->
                    Black
    in
    { topLeft = Matrix.transform matrix prev.topLeft
    , topRight = Matrix.transform matrix prev.topRight
    , botRight = Matrix.transform matrix prev.botRight
    , botLeft = Matrix.transform matrix prev.botLeft
    , fill = fill
    }


area : Block -> Float
area b =
    let
        dist : Coord -> Coord -> Float
        dist c1 c2 =
            sqrt <| (c2.x - c1.x) ^ 2 + (c2.y - c1.y) ^ 2

        base1 =
            dist b.topLeft b.topRight

        base2 =
            dist b.botLeft b.botRight

        height =
            dist b.topRight b.botRight
    in
    1 / 2 * (base1 + base2) * height


render : Block -> Svg.Svg msg
render { topLeft, topRight, botRight, botLeft, fill } =
    let
        paths =
            [ Move topLeft
            , Line topRight
            , Line botRight
            , Line botLeft
            , End
            ]
                |> List.map Path.render
                |> String.join " "
                |> SvgAttrs.d
    in
    Svg.path
        [ paths
        , SvgAttrs.fill <| Fill.render fill
        ]
        []
