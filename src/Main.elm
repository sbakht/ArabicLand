module Main exposing (Model, Msg(..), init, update, view)

import Browser
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import List as L exposing (map)
import Maybe as M
import String exposing (fromInt)
import Block exposing (Block, bFS, blockToString, blocksAtHeight, hasWord, height, insert, mkImplicitWordW, mkPlaceholder, mkWordW, testData)
import Word exposing (Word, mkImplicitWord, mkWord, wordToString)




---- MODEL ----


wordsArr =
    [ mkWord 1 "he", mkWord 2 "resisted", mkImplicitWord 2 "hidden he", mkWord 3 "the door", mkWord 4 "of the house" ]


y1 =
--    testData
    mkPlaceholder [ mkWordW 1 "he", mkWordW 2 "resisted", mkImplicitWordW 2 "hidden he", mkWordW 3 "the door", mkWordW 4 "of the house" ]

type alias Model =
    { block1 : Block, depth : Int }


init : Model
init =
     { block1 = y1, depth = 2 }



---- UPDATE ----


type Msg
    = NoOp
    | ClickTier Int
    | SetGrammar Word


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickTier i ->
            ( { model | depth = i }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
        SetGrammar word ->
           ({model | block1 = insert word (mkPlaceholder []) model.block1}, Cmd.none)


stringsEachSection : Int -> Block -> List (List String)
stringsEachSection depth block =
    map (\x -> blockToString x :: bFS wordToString x) <| blocksAtHeight depth block


wordsFrom : Block -> List Word
wordsFrom block =
    bFS (\a -> a) block



---- VIEW ----


view : Model -> Html Msg
view { depth, block1 } =
    div []
        [ img [ src "/logo.svg" ] []
        , div [] [ text (Debug.toString <| bFS wordToString block1) ]
        , viewWords (blocksAtHeight depth block1) wordsArr
        , text << Debug.toString <| stringsEachSection depth block1
        , viewBreakdown (blocksAtHeight depth block1)
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
    span [ class (grammarPlace word), onClick (SetGrammar word) ] [ text (wordToString word ++ " ") ]


viewBreakdown : List Block -> Html Msg
viewBreakdown blocks =
    div [] (viewWords blocks (L.concat <| L.map wordsFrom blocks) :: L.map viewPhraseBreak blocks)


viewPhraseBreak : Block -> Html Msg
viewPhraseBreak block =
    div [] [ span [ class "bold" ] [ text (blockToString block ++ ": ") ], viewBasicWords block ]


viewBasicWords : Block -> Html Msg
viewBasicWords block =
    span [] [ text (String.join " " <| L.map wordToString <| wordsFrom block) ]

