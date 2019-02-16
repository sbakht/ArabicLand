module Ch1 exposing (..)

import Html exposing (Html, text)

type alias Model = Int

init : Model
init = 0

type Msg
    = NoOp
update: Msg -> Model -> (Model, Cmd Msg)
update msg  model = (model, Cmd.none)

view : Model -> Html Msg
view model = text "hi"


