import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Animation exposing (px, deg)
import Mouse
import Array exposing (Array)
import Dict exposing (Dict)
import Window

-- APP
main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { styles : Array Animation.State
  , rotationValue : Int
  , sceneHeight : Int
  , sceneWidth : Int
  }


squareSide : number
squareSide = 100

-- INIT
init : (Model, Cmd Msg)
init =
  ( { styles =
      Array.fromList [1..50]
        |> Array.map (\x ->
          Animation.styleWithEach
            [ ( Animation.spring { stiffness = 180, damping = 21 }
              , Animation.translate (px 0.0) (px 0.0)
              )
            , ( Animation.spring { stiffness = 200, damping = 6 }
              , Animation.rotate (deg 0.0)
              )
            ]
        )
    , rotationValue = 0
    , sceneHeight = 0
    , sceneWidth = 0
    }
  , Cmd.none
  )



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Animation.subscription Animate (Array.toList model.styles)
    , Mouse.moves Go
    , Mouse.downs Rotate
    , Window.resizes SceneSize
    ]


-- UPDATE
type Msg
  = Go Mouse.Position
  | Rotate Mouse.Position
  | SceneSize Window.Size
  | Animate Animation.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Animate animMsg ->
      ( { model |
          styles = Array.map (\style -> Animation.update animMsg style) model.styles
        }
      , Cmd.none
      )

    SceneSize { width, height } ->
      ( { model |
          sceneWidth = width
        , sceneHeight = height
        }
      , Cmd.none
      )

    Go { x, y } ->
      let
        halfSide = squareSide / 2
        newStyles =
          model.styles
            |> Array.indexedMap (\index style ->
                Animation.interrupt
                  [ Animation.wait (20 * toFloat index)
                  , Animation.to
                    [ Animation.translate (px <| toFloat x - halfSide) (px <| toFloat y - halfSide) ]
                  ]
                  style
            )
      in
        ( { model |
            styles = newStyles
          }
        , Cmd.none
        )

    Rotate { x, y } ->
      let
        rotationValue = model.rotationValue + 90
        newStyles =
          model.styles
            |> Array.indexedMap (\index style ->
              Animation.interrupt
                [ Animation.wait (20 * toFloat index)
                , Animation.to
                  [ Animation.rotate (deg rotationValue) ]
                ]
                style
            )
      in
        ( { model |
            styles = newStyles
          , rotationValue = rotationValue
          }
        , Cmd.none
        )


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div
    [ style
      [ ("width", toString model.sceneWidth ++ "px")
      , ("height", toString model.sceneHeight ++ "px")
      ]
    ]
    ( model.styles
      |> Array.indexedMap (\index divStyle -> div
        ( Animation.render divStyle ++
          [ style
            [ ("width", toString squareSide ++ "px")
            , ("height", toString squareSide ++ "px")
            , ("background-color", "#F2" ++ blueHexaValue (Array.length model.styles) index ++ "67")
            , ("position", "absolute")
            ]
          ]
        )
        []
      )
      |> Array.toList
    )

blueHexaValue : Int -> Int -> String
blueHexaValue length index =
  let hexa =
    case 16 * index // length of
      10 -> "A"
      11 -> "B"
      12 -> "C"
      13 -> "D"
      14 -> "E"
      15 -> "F"
      x -> toString x
  in
    hexa ++ hexa
