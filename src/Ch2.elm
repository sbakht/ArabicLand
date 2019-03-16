module Ch2 exposing (Model, Msg, init, update, view)

import Element exposing (Attribute, Element, centerX, column, el, fill, html, htmlAttribute, layout, none, paragraph, rgb, row, width)
import Html exposing (Html, a, div, input, label, li, span, text, ul)
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, succeed)
import List exposing (concat)
import String exposing (fromInt)


type alias Model =
    { questions : List Exercise, apply : Maybe Answer, submitted : Bool }


type Answer
    = WordType WordType
    | GrammarType GrammarType


type alias Exercise =
    { text : String
    , questions : List QuestionType
    }


type QuestionType
    = Radio Answer YourAnswer


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


setAnswer : QuestionType -> YourAnswer -> QuestionType
setAnswer w ans =
    case w of
        Radio a _ ->
            Radio a ans


clearAnswer : QuestionType -> QuestionType
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
        (Decode.map2 Exercise
            (Decode.field "text" Decode.string)
            (Decode.field "questions" <|
                Decode.list
                    (Decode.map2 (\x y -> x y Nothing)
                        (Decode.field "type" questionTypeDecoder)
                        (Decode.field "answer" answerDecoder)
                    )
            )
        )


questionTypeDecoder : Decoder (Answer -> YourAnswer -> QuestionType)
questionTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "radio" ->
                        Decode.succeed Radio

                    _ ->
                        Decode.fail "Invalid question type"
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
    | OnAnswer Exercise QuestionType Answer
    | OnSubmit
    | OnRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetApplying a ->
            ( { model | apply = Just a }, Cmd.none )

        OnAnswer exercise r newAnswer ->
            let
                updateAnswers =
                    List.map
                        (\x ->
                            if x == exercise then
                                { x | questions = updated x.questions }

                            else
                                x
                        )
                        model.questions

                updated qTypes =
                    List.map
                        (\qt ->
                            if qt == r then
                                setAnswer r (Just newAnswer)

                            else
                                qt
                        )
                        qTypes
            in
            ( { model | questions = updateAnswers }, Cmd.none )

        OnSubmit ->
            ( { model | submitted = True }, Cmd.none )

        OnRestart ->
            ( model, Cmd.none )



---------------------------------------------------


view : Model -> Html Msg
view model =
    layout []
        (column [ width fill ]
            (if model.submitted then
                --                answerState model
                []

             else
                quizState model
            )
        )


quizState : Model -> List (Element Msg)
quizState model =
    [ html <| viewExercises model.submitted model.questions
    , el [ centerX ] <| viewSubmit
    ]



--
--
--answerState : Model -> List (Element Msg)
--answerState model =
--    [ viewQuestions model.submitted model.questions
--    , el [ centerX ] <| viewResults model.questions
--    ]
---------------------------------------------------


viewExercises : Bool -> List Exercise -> Html Msg
viewExercises bool xs =
    ul [] <| List.map viewExercise xs


viewExercise : Exercise -> Html Msg
viewExercise x =
    li [] [ viewXText x.text, viewQuestions x ]


viewXText : String -> Html Msg
viewXText s =
    span [] [ text s ]


viewQuestions : Exercise -> Html Msg
viewQuestions x =
    div [] <| List.map (viewQuestionType x) x.questions


viewQuestionType : Exercise -> QuestionType -> Html Msg
viewQuestionType x q =
    case q of
        Radio ans yrAns ->
            viewRadio x ans yrAns


viewRadio : Exercise -> Answer -> YourAnswer -> Html Msg
viewRadio x ans yourAns =
    case ans of
        WordType w ->
            div [] <| List.map (radio x ans "w" yourAns) <| getWordTypes

        GrammarType w ->
            div [] <| List.map (radio x ans "g" yourAns) <| getGrammarTypes


radio : Exercise -> Answer -> String -> YourAnswer -> Answer -> Html Msg
radio x ans rName yourAns val =
    label [] [ input [ type_ "radio", name rName, checked (yourAns == Just val), onInput (\_ -> OnAnswer x (Radio ans yourAns) val) ] [], text (toSymbol val) ]



---------------------------------------------------


viewResults : List Sentence -> Element Msg
viewResults questions =
    column []
        [ viewResultStatus questions
        , viewRestart
        ]


viewResultStatus : List Sentence -> Element Msg
viewResultStatus qs =
    Element.text ("You got " ++ fromInt (numCorrect qs) ++ "/" ++ fromInt (numQuestions qs) ++ " correct.")


viewSubmit : Element Msg
viewSubmit =
    html <| Html.button [ Html.Events.onClick OnSubmit, class "pure-button pure-button-primary" ] [ Html.text "Check Answers" ]


viewRestart : Element Msg
viewRestart =
    html <| Html.button [ Html.Events.onClick OnRestart, class "pure-button button-warning" ] [ Html.text "Start Over" ]



---------------------------------------------------


getWordTypes : List Answer
getWordTypes =
    List.map WordType [ Ism, Fil, Harf ]


getGrammarTypes : List Answer
getGrammarTypes =
    List.map GrammarType [ Rafa, Nasb, Jar ]


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


toSymbol : Answer -> String
toSymbol =
    answerSymbol << Just
