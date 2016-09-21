import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Animation exposing (px)
import Mouse

-- APP
main : Program Never
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- MODEL
type alias Model =
  { style : Animation.State
  , position : Position
  }

type Position = Back | Forward

-- INIT
init : (Model, Cmd Msg)
init =
  ( { style =
      Animation.style
        [ Animation.translate (px 0.0) (px 0.0) ]
    , position = Back
    }
  , Cmd.none
  )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Animation.subscription Animate [model.style]
    , Mouse.clicks Go
    ]


-- UPDATE
type Msg
  = GoForward
  | GoBack
  | Go { x : Int, y: Int }
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

    GoForward ->
      let
        newStyle =
          Animation.interrupt
            [ Animation.to
                [ Animation.translate (px 200.0) (px 200.0) ]
            ]
            model.style
      in

        ( { model |
            position = Forward,
            style = newStyle
          }
        , Cmd.none
        )

    GoBack ->
      let
        newStyle =
          Animation.interrupt
            [ Animation.to
                [ Animation.translate (px 0.0) (px 0.0) ]
            ]
            model.style
      in

        ( { model |
            position = Back,
            style = newStyle
          }
        , Cmd.none
        )
    Go { x, y }->
      let
        newStyle =
          Animation.interrupt
            [ Animation.to
                [ Animation.translate (px <| toFloat x) (px <| toFloat y) ]
            ]
            model.style
      in

        ( { model |
            position = Back,
            style = newStyle
          }
        , Cmd.none
        )


range : Int -> Int -> List Int
range start end =
  if start == end
    then [end]
    else [start] ++ range (start + 1) end


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div
    [ {-- onClick (if model.position == Back then GoForward else GoBack)
    , --} style
      [ ("width", "1000px")
      , ("height", "1000px")
      ]
    ]
    ( range 1 100
      |> List.map (\x -> div
        (Animation.render model.style ++
          [ style
            [ ("width", "50px")
            , ("height", "50px")
            , ("background-color", "blue")
            , ("position", "absolute")
            ]
          ]
        )
        []
      )
    )
