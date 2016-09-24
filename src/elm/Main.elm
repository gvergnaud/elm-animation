import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Animation exposing (px, deg)
import Mouse

import Utils.List exposing (range)

-- APP
main : Program Never
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- MODEL
type alias Model =
  { style : Animation.State
  , rotationValue : Int
  }

-- INIT
init : (Model, Cmd Msg)
init =
  ( { style = Animation.style
      [ Animation.translate (px 0.0) (px 0.0)
      , Animation.rotate (deg 0.0)
      ]
    , rotationValue = 0
    }
  , Cmd.none
  )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Animation.subscription Animate [model.style]
    , Mouse.moves Go
    , Mouse.clicks Rotate
    ]


-- UPDATE
type alias MousePosition = { x : Int, y: Int }

type Msg
  = Go MousePosition
  | Rotate MousePosition
  | Animate Animation.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Animate animMsg ->
      ( { model |
          style = Animation.update animMsg model.style
        }
      , Cmd.none
      )

    Go { x, y } ->
      let
        newStyle =
          Animation.interrupt
            [ Animation.to
              [ Animation.translate (px <| toFloat x) (px <| toFloat y) ]
            ]
            model.style
      in

        ( { model |
            style = newStyle
          }
        , Cmd.none
        )

    Rotate { x, y } ->
      let
        rotationValue = model.rotationValue + 90
        newStyle =
          Animation.interrupt
            [ Animation.to
              [ Animation.rotate (deg rotationValue) ]
            ]
            model.style
      in
        ( { model |
            style = newStyle
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
    [ {-- onClick (if model.style == Back then GoForward else GoBack)
    , --} style
      [ ("width", "1000px")
      , ("height", "1000px")
      ]
    ]
    ( range 1 1
      |> List.map (\x -> div
        ( Animation.render model.style ++
          [ style
            [ ("width", "50px")
            , ("height", "50px")
            , ("background-color", "#6dffff")
            , ("style", "absolute")
            ]
          ]
        )
        []
      )
    )
