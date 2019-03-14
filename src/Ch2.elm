module Ch2 exposing (Model, Msg, init, update, view)

import Element exposing (Attribute, Element, centerX, column, el, fill, html, htmlAttribute, layout, none, paragraph, rgb, row, text, width)
import Html exposing (Html, a, div, li, span, ul)
import Html.Attributes exposing (class, classList)
import Html.Events
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, succeed)
import List exposing (concat)
import String exposing (fromInt)


type alias Model =
    { questions : List Exercise, apply : Maybe Answer, submitted : Bool }


type Answer
    = WordType WordType
    | GrammarType GrammarType


type alias Exercise = {
    text: String,
    questions: List QuestionType
    }

type QuestionType = Radio Answer YourAnswer

type WordType
    = Ism
    | Fil
    | Harf


type GrammarType
    = Rafa
    | Nasb
    | Jar


type alias YourAnswer =
    Maybe Answer


type alias Submitted =
    Bool


type alias Sentence =
    List Word


type Word
    = Question String Answer YourAnswer
    | Text String


isCorrect : Word -> Maybe Bool
isCorrect w =
    case w of
        Question _ a a1 ->
            Just <| Just a == a1

        Text _ ->
            Nothing


setAnswer : Word -> YourAnswer -> Word
setAnswer w ans =
    case w of
        Question s a _ ->
            Question s a ans

        Text s ->
            Text s


clearAnswer : Word -> Word
clearAnswer q =
    setAnswer q Nothing


numCorrect : List Sentence -> Int
numCorrect =
    List.length << List.filter (\x -> x) << List.filterMap isCorrect << concat


numQuestions : List Sentence -> Int
numQuestions =
    List.length << List.filter isQuestion << concat


isQuestion : Word -> Bool
isQuestion w =
    case w of
        Question _ _ _ ->
            True

        Text _ ->
            False


questionDecoder : Decoder (List Exercise)
questionDecoder =
    Decode.list
        ( Decode.map2 Exercise
            (Decode.field "text" Decode.string)
            (Decode.field  "questions" <| Decode.list
                 ( Decode.map2 (\x y -> x y Nothing)
                    (Decode.field "type" questionTypeDecoder )
                    (Decode.field "answer" answerDecoder)
                 )
             )
        )

questionTypeDecoder : Decoder (Answer -> YourAnswer -> QuestionType)
questionTypeDecoder =
    Decode.string |>
        Decode.andThen
            (\str -> case str of
                "radio" -> Decode.succeed Radio
                _ -> Decode.fail "Invalid question type"
            )


answerDecoder : Decoder Answer
answerDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "I" ->
                        Decode.succeed <| WordType Ism

                    "F" ->
                        Decode.succeed <| WordType Fil

                    "H" ->
                        Decode.succeed <| WordType Harf

                    "R" ->
                        Decode.succeed <| GrammarType Rafa

                    "N" ->
                        Decode.succeed <| GrammarType Nasb

                    "J" ->
                        Decode.succeed <| GrammarType Jar

                    _ ->
                        Decode.fail "Invalid answer choice"
            )


init : Value -> Model
init v =
    { questions = Debug.log "test" <| Result.withDefault [] (decodeValue questionDecoder v), apply = Nothing, submitted = False }


type Msg
    = SetApplying Answer
--    | OnAnswer Word YourAnswer
    | OnSubmit
    | OnRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetApplying a ->
            ( { model | apply = Just a }, Cmd.none )

--        OnAnswer question answer ->
--            let
--                updateAnswers =
--                    List.map
--                        (List.map
--                            (\q ->
--                                if q == question then
--                                    setAnswer q answer
--
--                                else
--                                    q
--                            )
--                        )
--                        model.questions
--            in
--            ( { model | questions = updateAnswers }, Cmd.none )

        OnSubmit ->
            ( { model | submitted = True }, Cmd.none )

        OnRestart ->
            (model, Cmd.none)



---------------------------------------------------


view : Model -> Html Msg
view model =
    layout []
        (column [ width fill ]
            (if model.submitted then
--                answerState model
                    []

             else
--                quizState model
                    []
            )
        )

--
--quizState : Model -> List (Element Msg)
--quizState model =
--    [ viewQuestions model.submitted model.questions
--    , el [ centerX ] <| viewSubmit
--    ]
--
--
--answerState : Model -> List (Element Msg)
--answerState model =
--    [ viewQuestions model.submitted model.questions
--    , el [ centerX ] <| viewResults model.questions
--    ]



---------------------------------------------------






---------------------------------------------------


viewResults : List Sentence -> Element Msg
viewResults questions =
    column []
        [ viewResultStatus questions
        , viewRestart
        ]


viewResultStatus : List Sentence -> Element Msg
viewResultStatus qs =
    text ("You got " ++ fromInt (numCorrect qs) ++ "/" ++ fromInt (numQuestions qs) ++ " correct.")


viewSubmit : Element Msg
viewSubmit =
    html <| Html.button [ Html.Events.onClick OnSubmit, class "pure-button pure-button-primary" ] [ Html.text "Check Answers" ]


viewRestart : Element Msg
viewRestart =
    html <| Html.button [ Html.Events.onClick OnRestart, class "pure-button button-warning" ] [ Html.text "Start Over" ]



---------------------------------------------------




answerToString : Answer -> String
answerToString answer =
    case answer of
        WordType Ism ->
            "Ism"

        WordType Fil ->
            "Fil"

        WordType Harf ->
            "Harf"

        GrammarType Rafa ->
            "Rafa"

        GrammarType Nasb ->
            "Nasb"

        GrammarType Jar ->
            "Jar"


answerSymbol : YourAnswer -> String
answerSymbol a =
    case a of
        Just (WordType Ism) ->
            "I"

        Just (WordType Fil) ->
            "F"

        Just (WordType Harf) ->
            "H"

        Just (GrammarType Rafa) ->
            "R"

        Just (GrammarType Nasb) ->
            "N"

        Just (GrammarType Jar) ->
            "J"

        Nothing ->
            "?"

