module Block exposing (Block, bFS, blockToString, blocksAtHeight, enqueueBlock, hasWord, height, mkImplicitWordW, mkWordW, testData, mkPlaceholder, insert)

import List exposing (any, append, concat, foldl, foldr, map, maximum, member)
import Maybe exposing (withDefault)
import Queue exposing (Queue, dequeue, emptyQueue, enqueue)
import Word exposing (Word, mkImplicitWord, mkWord)


type Block
    = Nominal Block Block
    | Verbal Block Block Block
    | Fil Block
    | Fial Block
    | Word Word
    | Mubtada Block
    | Kabr Block
    | MB Block
    | Placeholder (List Block)


testData =
    Nominal (Mubtada <| mkWordW 1 "he") (Kabr (Verbal (Fil <| mkWordW 2 "resisted") (Fial <| mkImplicitWordW 2 "hidden he") (MB <| Placeholder [(Mubtada <| mkWordW 3 "the door"), (Kabr <| mkWordW 4 "of the house")])))

mkPlaceholder : List Block -> Block
mkPlaceholder = Placeholder

mkWordW : Int -> String -> Block
mkWordW id text =
    Word <| mkWord id text


mkImplicitWordW : Int -> String -> Block
mkImplicitWordW id text =
    Word <| mkImplicitWord id text



--mkConnectorWordW : Int -> String -> Block
--mkConnectorWordW id text =
--    Word <| ConnectorWord { id = id, text = text }


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

        Placeholder ws ->
            foldl (\w qu -> enqueue w qu) q ws


height : Block -> Int
height block =
    case block of
        Nominal b1 b2 ->
            max (height b1) (height b2)

        Verbal b1 b2 b3 ->
            max (height b3) (max (height b1) (height b2))

        Placeholder bs ->
            withDefault 1 <| maximum (map height bs)

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
        Placeholder _ ->
            "PLACEHOLDER"


blocksAtHeight : Int -> Block -> List Block
blocksAtHeight height1 bl =
    let
        go h block =
            case ( h, block ) of
                ( _, Nominal b1 b2 ) ->
                    go h b1 ++ go h b2

                ( _, Verbal b1 b2 b3 ) ->
                    go h b1 ++ go h b2 ++ go h b3

                (_, Placeholder bs) ->
                    foldr (\b accum -> append (go h b) accum) [] bs

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

                ( Nothing, q2 ) ->
                    if q2 == emptyQueue then
                        []

                    else
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

        Placeholder ws ->
            any (hasWord word) ws

insert : Word -> Block -> Block -> Block
insert word block main =
    if block == main || Word word == main then
--        Debug.log "hmm" <| addWord word block
        addWord word block
    else
        case main of
            Nominal b1 b2 ->
                Nominal ( insert word block b1) ( insert word block b2)

            Verbal b1 b2 b3 ->
                Verbal ( insert word block b1) ( insert word block b2) ( insert word block b3)

            Fil b ->
                Fil <| insert word block b

            Fial b ->
                Fial <| insert word block b

            Mubtada b ->
                Mubtada <| insert word block b

            Kabr b ->
                Kabr <| insert word block b

            MB b ->
                MB <| insert word block b

            Word w ->
                Word w

            Placeholder ws ->
                Placeholder (map (insert word block) ws)

addWord : Word -> Block -> Block
addWord word block =
    case block of
        Placeholder ws ->
            Placeholder (Word word :: ws)

        x ->
            Placeholder [Word word, x]


