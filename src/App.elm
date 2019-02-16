module App exposing (..)

import Browser
import Html exposing (Html, text)
import Ch1
import Ch2
import Main as Grammar

type alias Model = {page : Page}
type Msg = Ch1Msg Ch1.Msg | Ch2Msg Ch2.Msg | GrammarMsg Grammar.Msg | NoOp

type Page = Ch1Model Ch1.Model | Ch2Model Ch2.Model | GrammarModel Grammar.Model | Default Int



init : String -> (Model, Cmd Msg)
init chapter = case chapter of
    "ch1" ->
        ({page =  Ch1Model Ch1.init }, Cmd.none)
    "ch2" ->
        ({page =  Ch2Model Ch2.init }, Cmd.none)
    "grammar" ->
        ({page =  GrammarModel Grammar.init }, Cmd.none)
    _ ->
        ({page =  Default 0}, Cmd.none)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = (model, Cmd.none)

view : Model -> Html Msg
view model =
    case model.page of
            Ch1Model subModel ->
                Html.map Ch1Msg <| Ch1.view subModel

            Ch2Model subModel ->
                Html.map Ch2Msg <| Ch2.view subModel

            GrammarModel subModel ->
                Html.map GrammarMsg <| Grammar.view subModel

            Default _ ->
                text ""


main : Program String Model Msg
main =
    Browser.element
        {
          view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
