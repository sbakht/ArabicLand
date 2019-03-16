module Ch2 exposing (Model, Msg, init, update, view)

import Element exposing (Attribute, Element, centerX, column, el, fill, html, layout, width)
import Html exposing (Html, a, div, input, label, li, span, table, td, text, tr, ul)
import Html.Attributes exposing (checked, class, name, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import List exposing (concat)
import String exposing (fromInt)


type alias Model =
    { exercises : List Exercise, apply : Maybe Answer, submitted : Bool }


type Answer
    = WordType WordType
    | GrammarType GrammarType


type alias Exercise =
    { text : String
    , questions : List QuestionType
    , id : Int
    }


type alias Question =
    { questionType : QuestionType
    , id : Int
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


type ID
    = ID Int Int


type alias YourAnswer =
    Maybe Answer



------- Helper Alias


type alias Submitted =
    Bool


type alias RadioName =
    String


type alias RadioValue =
    Answer


type alias RadioType =
    Answer



--------
--type alias Sentence =
--    List Word
--type Word
--    = Question String Answer YourAnswer
--    | Text String
--isCorrect : Word -> Maybe Bool
--isCorrect w =
--    case w of
--        Question _ a a1 ->
--            Just <| Just a == a1
--
--        Text _ ->
--            Nothing


setAnswer : QuestionType -> YourAnswer -> QuestionType
setAnswer w ans =
    case w of
        Radio a _ ->
            Radio a ans


clearAnswer : QuestionType -> QuestionType
clearAnswer q =
    setAnswer q Nothing



--numCorrect : List Sentence -> Int
--numCorrect =
--    List.length << List.filter (\x -> x) << List.filterMap isCorrect << concat
--numQuestions : List Sentence -> Int
--numQuestions =
--    List.length << List.filter isQuestion << concat


mkIDs : List Exercise -> List Exercise
mkIDs =
    List.indexedMap (\i x -> { x | id = i })


questionDecoder : Decoder (List Exercise)
questionDecoder =
    Decode.list
        (Decode.map3 Exercise
            (Decode.field "text" Decode.string)
            (Decode.field "questions" <|
                Decode.list
                    (Decode.map2 (\x y -> x y Nothing)
                        (Decode.field "type" questionTypeDecoder)
                        (Decode.field "answer" answerDecoder)
                    )
            )
            (Decode.succeed -1)
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
    { exercises = mkIDs <| Result.withDefault [] (decodeValue questionDecoder v), apply = Nothing, submitted = False }


type Msg
    = SetApplying Answer
    | OnAnswer ID Answer
    | OnSubmit
    | OnRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetApplying a ->
            ( { model | apply = Just a }, Cmd.none )

        OnAnswer (ID exerciseID questionID) newAnswer ->
            let
                updateExercise =
                    List.map
                        (\x ->
                            if x.id == exerciseID then
                                { x | questions = updateAnswer x.questions }

                            else
                                x
                        )
                        model.exercises

                updateAnswer qTypes =
                    List.indexedMap
                        (\qID qt ->
                            if qID == questionID then
                                setAnswer qt (Just newAnswer)

                            else
                                qt
                        )
                        qTypes
            in
            ( { model | exercises = updateExercise }, Cmd.none )

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
    [ html <| viewExercises model.submitted model.exercises
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
    table [] <| List.map viewExercise xs


viewExercise : Exercise -> Html Msg
viewExercise x =
    tr [] ( viewXText x.text :: viewQuestions x )


viewXText : String -> Html Msg
viewXText s =
    td [] [span [] [ text s ]]


viewQuestions : Exercise -> List (Html Msg)
viewQuestions x =
    List.indexedMap (\i a -> viewQuestion (ID x.id i) a) x.questions


viewQuestion : ID -> QuestionType -> Html Msg
viewQuestion id q =
    let
        go =
            case q of
                Radio ans yrAns ->
                    viewRadio id ans yrAns
    in
    td [] [go]


viewRadio : ID -> RadioType -> YourAnswer -> Html Msg
viewRadio id ans yourAns =
    let
        mkRadioGroup : List RadioValue -> Html Msg
        mkRadioGroup =
            div [] << List.map (radio id (idToString id) yourAns)
    in
    case ans of
        WordType _ ->
            mkRadioGroup getWordTypes

        GrammarType _ ->
            mkRadioGroup getGrammarTypes


radio : ID -> RadioName -> YourAnswer -> RadioValue -> Html Msg
radio id rName selected val =
    label [] [ input [ type_ "radio", name rName, checked (selected == Just val), onClick (OnAnswer id val) ] [], text (toSymbol val) ]



---------------------------------------------------
--viewResults : List Sentence -> Element Msg
--viewResults questions =
--    column []
--        [ viewResultStatus questions
--        , viewRestart
--        ]
--viewResultStatus : List Sentence -> Element Msg
--viewResultStatus qs =
--    Element.text ("You got " ++ fromInt (numCorrect qs) ++ "/" ++ fromInt (numQuestions qs) ++ " correct.")


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


idToString : ID -> String
idToString (ID i j) =
    fromInt i ++ "-" ++ fromInt j


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
