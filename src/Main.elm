port module Main exposing (main)

import Block exposing (Block)
import Browser exposing (Document)
import Browser.Dom as Browser exposing (Viewport)
import Browser.Events as Browser
import Coord exposing (Coord)
import Fill exposing (Fill(..))
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Task



-- PORTS


port onScroll : (Float -> msg) -> Sub msg



-- MODEL


type alias Model =
    { width : Float
    , height : Float
    , zoom : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        -- Get the viewport (width, height)
        parseViewport : Viewport -> ( Float, Float )
        parseViewport { scene } =
            ( scene.width, scene.height )
    in
    ( { width = 0
      , height = 0
      , zoom = 0
      }
    , Task.perform (parseViewport >> WindowResize) Browser.getViewport
    )



-- UPDATE


type Msg
    = WindowResize ( Float, Float )
    | Zoom Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize ( width, height ) ->
            ( { model | width = width, height = height }, Cmd.none )

        Zoom _ ->
            let
                maxDim =
                    max model.width model.height

                zoomStep =
                    10

                -- After this much "zoom", fractal looks the same as when we started
                zoomReset =
                    round <| 3 / 8 * maxDim

                newZoom =
                    remainderBy zoomReset (model.zoom + zoomStep)
            in
            ( { model | zoom = newZoom }, Cmd.none )



-- SUBSCRIPTIONS


{-| Subscribe to:

1.  window resizing events, so we can re-compute our viewport
2.  scroll events, so we can perform a "zoom"

-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.onResize (\width height -> WindowResize ( toFloat width, toFloat height ))
        , onScroll Zoom
        ]



-- VIEW


{-| Generate a spiral from a seed block. Recursively compute the next block in the spiral, until
they are too smal to be visible, at which point, stop generating blocks and complete the spiral.
The entire fractal is composed of four individual spirals, the seed-blocks for which should be
picked correctly for the spirals to "fit" together.
-}
spiral : Block -> List Block
spiral seed =
    let
        epsilon =
            0.01

        block =
            Block.next seed

        area =
            Block.area block
    in
    -- Blocks are too small to be seen, so we are done generating them.
    if area <= epsilon then
        [ block ]

    else
        seed :: spiral block


{-| Determine the view box of the entire SVG image. There are two goals:

1.  center the fractal in the visible area. We accomplish this by making the image as large as the
    maxDim, but adjusting the view box to use the minDim.
2.  allow "zooming" into the fractal. We accomplish this by scaling the view box to make it look
    like zoom is happening, but resetting it whenever we are at the "original" starting image.

-}
viewBox : Model -> String
viewBox model =
    let
        maxDim =
            max model.width model.height

        zoomAdj =
            toFloat model.zoom
    in
    [ zoomAdj
    , zoomAdj
    , maxDim - 2 * zoomAdj
    , maxDim - 2 * zoomAdj
    ]
        |> List.map String.fromFloat
        |> String.join " "


view : Model -> Document Msg
view model =
    let
        -- Normalized "size" of a block (specifically, the height of the block at (0,0)) from which
        -- all other block dimentions/positions/scaling is calculated.
        a : Float
        a =
            max model.width model.height / 4

        seeds =
            [ { topLeft = Coord (-2 * a) (2 * a)
              , topRight = Coord 0 0
              , botRight = Coord a a
              , botLeft = Coord 0 (2 * a)
              , fill = Black
              }
            , { topLeft = Coord (-2 * a) (-2 * a)
              , topRight = Coord (2 * a) (-2 * a)
              , botRight = Coord (2 * a) 0
              , botLeft = Coord 0 0
              , fill = Black
              }
            , { topLeft = Coord (6 * a) (-2 * a)
              , topRight = Coord (6 * a) (2 * a)
              , botRight = Coord (4 * a) (2 * a)
              , botLeft = Coord (4 * a) 0
              , fill = White
              }
            , { topLeft = Coord (6 * a) (6 * a)
              , topRight = Coord (2 * a) (6 * a)
              , botRight = Coord (2 * a) (4 * a)
              , botLeft = Coord (4 * a) (4 * a)
              , fill = Black
              }
            ]

        blocks : List (Svg Msg)
        blocks =
            seeds
                |> List.map spiral
                |> List.concat
                |> List.map Block.render
    in
    { title = "Infinite chessboard"
    , body =
        [ Svg.svg
            [ Svg.width <| String.fromFloat <| max model.width model.height
            , Svg.height <| String.fromFloat <| max model.width model.height
            , Svg.viewBox <| viewBox model
            ]
            blocks
        ]
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }
