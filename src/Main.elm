module Main exposing (Block(..), Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import List as L exposing (drop, head, tail)



---- MODEL ----


type alias Model =
    {}


type Block
    = Nominal Block Block
    | Verbal Block Block (List Block)
    | Fil Block
    | Fial Block
    | Word Word
    | Mubtada Block
    | Kabr Block
    | MB Block

type Word = NormalWord String | HiddenWord | ConnectorWord


--z = "he is cool"
--z1 = Nominal (mkWord "he") (mkWord "cool")

mkWord : String -> Block
mkWord = Word << NormalWord

--y = "he resisted the fluff"
--y1 = Nominal (Mubtada <| mkWord "he") (Kabr (Verbal (Fil <| mkWord "resisted") (Fial <| mkWord "hidden he") [MB <| mkWord "the fluff"]))


enqueue : a -> List a -> List a
enqueue x xs = L.append xs [x]

dequeue : List a -> (Maybe a, List a)
dequeue xs = (head xs, drop 1 xs)

--enqueueBlock : Block -> List Block -> List Block
--enqueueBlock x xs = case x of
--    Nominal b1 b2 ->

breathFirstSearch : List a -> List Int
breathFirstSearch xs = []



init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
