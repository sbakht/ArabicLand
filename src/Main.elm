module Main exposing (Block(..), Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, div, h1, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List as L exposing (drop, head, map, tail)
import Maybe as M
import String exposing (fromInt)



---- MODEL ----


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
    = NormalWord { id : Int, text : String }
    | HiddenWord { id : Int, text : String }
    | ConnectorWord { id : Int, text : String }


mkWord : Int -> String -> Word
mkWord id text =
    NormalWord { id = id, text = text }


mkImplicitWord : Int -> String -> Word
mkImplicitWord id text =
    HiddenWord { id = id, text = text }


mkWordW : Int -> String -> Block
mkWordW id text =
    Word <| NormalWord { id = id, text = text }


mkImplicitWordW : Int -> String -> Block
mkImplicitWordW id text =
    Word <| HiddenWord { id = id, text = text }


mkConnectorWordW : Int -> String -> Block
mkConnectorWordW id text =
    Word <| ConnectorWord { id = id, text = text }


z =
    "he is cool"



--z1 =
--    Nominal (Mubtada <| mkWord "he") (Kabr <| mkWord "cool")
--y = "he resisted the fluff"


wordsArr =
    [ mkWord 1 "he", mkWord 2 "resisted", mkImplicitWord 2 "hidden he", mkWord 3 "the fluff" ]


y1 =
    Nominal (Mubtada <| mkWordW 1 "he") (Kabr (Verbal (Fil <| mkWordW 2 "resisted") (Fial <| mkImplicitWordW 2 "hidden he") (MB <| mkWordW 3 "the fluff")))


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
            x.text

        HiddenWord _ ->
            "HIDDEN"

        ConnectorWord _ ->
            "CONNECTOR"


bFS : (Word -> a) -> Block -> List a
bFS fn block =
    let
        go : Queue Block -> List a
        go q =
            case dequeue q of
                ( Just dequeued, q2 ) ->
                    case dequeued of
                        Word word ->
                            fn word :: go q2

                        b ->
                            go (enqueueBlock b q2)

                ( Nothing, Queue [] ) ->
                    []

                ( Nothing, q2 ) ->
                    go q2
    in
    go (enqueueBlock block emptyQueue)


hasWord : Word -> Block -> Bool
hasWord word block =
    case block of
        Nominal b1 b2 ->
            hasWord word b1 || hasWord word b2

        Verbal b1 b2 b3 ->
            hasWord word b1 || hasWord word b2 || hasWord word b3

        Fil b ->
            hasWord word b

        Fial b ->
            hasWord word b

        Mubtada b ->
            hasWord word b

        Kabr b ->
            hasWord word b

        MB b ->
            hasWord word b

        Word w ->
            word == w


height : Block -> Int
height block =
    case block of
        Nominal b1 b2 ->
            0 + max (height b1) (height b2)

        Verbal b1 b2 b3 ->
            0 + max (height b3) (max (height b1) (height b2))

        Fil b ->
            1 + height b

        Fial b ->
            1 + height b

        Mubtada b ->
            1 + height b

        Kabr b ->
            1 + height b

        MB b ->
            1 + height b

        Word _ ->
            0


blockToString : Block -> String
blockToString block =
    case block of
        Mubtada _ ->
            "Mubtada"

        Kabr _ ->
            "Kabr"

        Fil _ ->
            "Fil"

        Fial _ ->
            "Fial"

        MB _ ->
            "Mafoo_bihi"

        Nominal _ _ ->
            "Nominal"

        Verbal _ _ _ ->
            "Verbal"

        Word _ ->
            "Word"


blocksAtHeight : Int -> Block -> List Block
blocksAtHeight height1 bl =
    let
        go h block =
            case ( h, block ) of
                ( _, Nominal b1 b2 ) ->
                    go h b1 ++ go h b2

                ( _, Verbal b1 b2 b3 ) ->
                    go h b1 ++ go h b2 ++ go h b3

                ( _, Word _ ) ->
                    []

                ( 0, b ) ->
                    [ b ]

                ( _, Fil b ) ->
                    go (h - 1) b

                ( _, Fial b ) ->
                    go (h - 1) b

                ( _, Mubtada b ) ->
                    go (h - 1) b

                ( _, Kabr b ) ->
                    go (h - 1) b

                ( _, MB b ) ->
                    go (h - 1) b
    in
    go (height1 - 1) bl


type alias Model =
    { block1 : Block, depth : Int }


init : ( Model, Cmd Msg )
init =
    ( { block1 = y1, depth = 2 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ClickTier Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickTier i ->
            ( { model | depth = i }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


stringsEachSection : Int -> Block -> List (List String)
stringsEachSection depth block =
    map (\x -> blockToString x :: bFS wordToString x) <| blocksAtHeight depth block


wordsEachSection : Int -> Block -> List (List Word)
wordsEachSection depth block =
    map (\x -> bFS (\a -> a) x) <| blocksAtHeight depth block



---- VIEW ----


view : Model -> Html Msg
view { depth, block1 } =
    div []
        [ img [ src "/logo.svg" ] []
        , div [] [ text (Debug.toString <| bFS wordToString block1) ]
        , viewWords (blocksAtHeight depth block1) wordsArr
        , text << Debug.toString <| stringsEachSection depth block1
        , viewTierLinks (height block1)
        ]


viewTierLinks : Int -> Html Msg
viewTierLinks x =
    div [] (map viewTierLink (L.range 1 x))


viewTierLink : Int -> Html Msg
viewTierLink i =
    div [] [ a [ onClick <| ClickTier i ] [ tierText i ] ]


tierText : Int -> Html Msg
tierText i =
    text ("Tier " ++ fromInt i)


viewWords : List Block -> List Word -> Html Msg
viewWords blocks words =
    div [ class "words" ] <| L.map (viewWord blocks) words


viewWord : List Block -> Word -> Html Msg
viewWord blocks word =
    let
        grammarPlace : Word -> String
        grammarPlace w =
            L.filter (hasWord w) blocks
                |> L.head
                |> M.map blockToString
                |> M.withDefault ""
    in
    span [ class (grammarPlace word) ] [ text (wordToString word ++ " ") ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
