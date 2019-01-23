module Main exposing (Block(..), Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, div, h1, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List as L exposing (drop, head, tail)
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
    = NormalWord String
    | HiddenWord String
    | ConnectorWord String


mkWord : String -> Block
mkWord =
    Word << NormalWord


mkImplicitWord : String -> Block
mkImplicitWord =
    Word << HiddenWord


mkConnectorWord : String -> Block
mkConnectorWord =
    Word << ConnectorWord


z =
    "he is cool"


z1 =
    Nominal (Mubtada <| mkWord "he") (Kabr <| mkWord "cool")



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
            "Mafoo bihi"

        Nominal _ _ ->
            "Nominal"

        Verbal _ _ _ ->
            "Verbal"

        Word _ ->
            "Word"


bFS2 : Int -> Block -> List Block
bFS2 height1 bl =
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



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [] [ text (Debug.toString <| bFS model.block1) ]
        , fancy (bFS model.block1) (L.map (\x -> blockToString x :: bFS x) <| bFS2 model.depth model.block1)
        , text << Debug.toString << L.map (\x -> blockToString x :: bFS x) <| bFS2 model.depth model.block1
        , viewTierLinks (height model.block1)
        ]


viewTierLinks : Int -> Html Msg
viewTierLinks x =
    div [] <| L.map (\i -> a [ onClick <| ClickTier i ] [ text <| "Tier " ++ fromInt i ]) (L.range 1 x)


fancy : List String -> List (List String) -> Html Msg
fancy xs xxs =
    let
        at : String -> Int
        at s =
            Maybe.withDefault 100 <| L.head <| L.map (\( i, x ) -> i) <| L.filter (\( i, x ) -> L.member s x) <| L.indexedMap (\i x -> ( i, x )) xxs
    in
    div [ class "words" ] (L.map (\s -> (\i -> span [ class <| "h" ++ i ] [ text <| s ++ " " ]) << fromInt <| at s) xs)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
