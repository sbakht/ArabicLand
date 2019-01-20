module Main exposing (Block(..), Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import List as L exposing (drop, head, tail)



---- MODEL ----


type alias Model =
    { block : Block }


type Block
    = Nominal Block Block
    | Verbal Block Block Block
    | Fil Block
    | Fial Block
    | Word Word
    | Mubtada Block
    | Kabr Block
    | MB Block


type Word
    = NormalWord String
    | HiddenWord String
    | ConnectorWord String


z =
    "he is cool"


z1 =
    Nominal (mkWord "he") (mkWord "cool")


mkWord : String -> Block
mkWord =
    Word << NormalWord


mkImplicitWord : String -> Block
mkImplicitWord =
    Word << HiddenWord


mkConnectorWord : String -> Block
mkConnectorWord =
    Word << ConnectorWord



--y = "he resisted the fluff"


y1 =
    Nominal (Mubtada <| mkWord "he") (Kabr (Verbal (Fil <| mkWord "resisted") (Fial <| mkImplicitWord "hidden he") (MB <| mkWord "the fluff")))


type Queue a
    = Queue (List a)


initQueue : a -> Queue a
initQueue a =
    enqueue a (Queue [])


emptyQueue : Queue a
emptyQueue =
    Queue []


mapQ : (List a -> List a) -> Queue a -> Queue a
mapQ fn (Queue xs) =
    Queue (fn xs)


enqueue : a -> Queue a -> Queue a
enqueue x q =
    mapQ (\xs -> L.append xs [ x ]) q


dequeue : Queue a -> ( Maybe a, Queue a )
dequeue (Queue xs) =
    ( head xs, Queue (drop 1 xs) )


enqueueBlock : Block -> Queue Block -> Queue Block
enqueueBlock x q =
    case x of
        Nominal b1 b2 ->
            enqueue b2 (enqueue b1 q)

        Verbal b1 b2 b3 ->
            enqueue b3 (enqueue b2 (enqueue b1 q))

        Fil b ->
            enqueue b q

        Fial b ->
            enqueue b q

        Mubtada b ->
            enqueue b q

        Kabr b ->
            enqueue b q

        MB b ->
            enqueue b q

        Word w ->
            enqueue (Word w) q


wordToString : Word -> String
wordToString word =
    case word of
        NormalWord x ->
            x

        HiddenWord _ ->
            "HIDDEN"

        ConnectorWord _ ->
            "CONNECTOR"


bFS : Block -> List String
bFS block =
    let
        go : Queue Block -> List String
        go q =
            case dequeue q of
                ( Just dequeued, q2 ) ->
                    case dequeued of
                        Word word ->
                            wordToString word :: go q2

                        b ->
                            go (enqueueBlock b q2)

                ( Nothing, Queue [] ) ->
                    []

                ( Nothing, q2 ) ->
                    go q2
    in
    go (enqueueBlock block emptyQueue)


init : ( Model, Cmd Msg )
init =
    ( { block = y1 }, Cmd.none )



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
        , text (Debug.toString <| bFS model.block)
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
