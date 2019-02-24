module App exposing (Model, Msg(..), Page(..), init, main, update, updatePage, view)

import Browser
import Ch1
import Ch2
import Html exposing (Html, text)
import Json.Decode exposing (Value)
import Main as Grammar


type alias Model =
    { page : Page }


type Msg
    = Ch1Msg Ch1.Msg
    | Ch2Msg Ch2.Msg
    | GrammarMsg Grammar.Msg
    | NoOp


type Page
    = Ch1Model Ch1.Model
    | Ch2Model Ch2.Model
    | GrammarModel Grammar.Model
    | Default Int

type alias Flag = {ch: String, data: Value}

init : Flag -> ( Model, Cmd Msg )
init chapter =
    case chapter.ch of
        "ch1" ->
            ( { page = Ch1Model <| Ch1.init chapter.data }, Cmd.none )

        "ch2" ->
            ( { page = Ch2Model Ch2.init }, Cmd.none )

        "grammar" ->
            ( { page = GrammarModel Grammar.init }, Cmd.none )

        _ ->
            ( { page = Default 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage model.page msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | page = toModel newModel }, Cmd.map toMsg newCmd )
    in
    case ( msg, page ) of
        ( GrammarMsg subMsg, GrammarModel subModel ) ->
            toPage GrammarModel GrammarMsg Grammar.update subMsg subModel
        ( Ch1Msg subMsg, Ch1Model subModel ) ->
            toPage Ch1Model Ch1Msg Ch1.update subMsg subModel

        ( _, _ ) ->
            ( model, Cmd.none )


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


main : Program Flag Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
