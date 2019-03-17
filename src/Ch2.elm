module Ch2 exposing (Model, Msg, init, update, view)

import Element exposing (Attribute, Element, centerX, column, el, fill, html, layout, width)
import Html exposing (Html, a, div, input, label, li, span, table, td, text, tr, ul)
import Html.Attributes exposing (checked, class, name, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Json.Decode.Pipeline exposing (hardcoded, required)
import List exposing (concat)
import String exposing (fromInt)


type alias Model =
    { exercises : List Exercise, apply : Maybe RadioAnswer, submitted : Bool }


type RadioAnswer
    = WordType WordType
    | GrammarType GrammarType


type InputType
    = InputType String


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
    = RadioQuestion Radio
    | WriteQuestion Write


type Radio
    = Radio RadioAnswer YourRadioAnswer


type Write
    = Write InputType (Maybe InputType)


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


type alias YourRadioAnswer =
    Maybe RadioAnswer



------- Helper Alias


type alias Submitted =
    Bool


type alias RadioName =
    String


type alias RadioValue =
    RadioAnswer



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
--setAnswer : QuestionType -> YourAnswer -> QuestionType
--setAnswer w ans =
--    case w of
--        RadioQuestion (Radio a _) ->
--            RadioQuestion <| Radio a ans
----        Write a _ ->
----            Write a ans


setRadioAnswer : Radio -> YourRadioAnswer -> QuestionType
setRadioAnswer (Radio a _) ans =
    RadioQuestion <| Radio a ans


setWriteAnswer : Write -> Maybe InputType -> QuestionType
setWriteAnswer (Write a _) ans =
    WriteQuestion <| Write a ans



--clearAnswer : QuestionType -> QuestionType
--clearAnswer q =
--    setAnswer q Nothing
--numCorrect : List Sentence -> Int
--numCorrect =
--    List.length << List.filter (\x -> x) << List.filterMap isCorrect << concat
--numQuestions : List Sentence -> Int
--numQuestions =
--    List.length << List.filter isQuestion << concat


mkIDs : List Exercise -> List Exercise
mkIDs =
    List.indexedMap (\i x -> { x | id = i })


exercisesDecoder : Decoder (List Exercise)
exercisesDecoder =
    Decode.list
        (Decode.succeed Exercise
            |> required "text" Decode.string
            |> required "questions" questionsDecoder
            |> hardcoded -1
        )


questionsDecoder : Decoder (List QuestionType)
questionsDecoder =
    Decode.list questionDecoder


questionDecoder : Decoder QuestionType
questionDecoder =
    Decode.andThen questionFromType (Decode.field "type" Decode.string)


questionFromType : String -> Decoder QuestionType
questionFromType str =
    case str of
        "radio" ->
            Decode.map (\ans -> RadioQuestion <| Radio ans Nothing)
                (Decode.andThen radioAnswerFromSymbol answerDecoder)

        "input" ->
            Decode.map (\ans -> WriteQuestion <| Write ans Nothing)
                (Decode.map InputType answerDecoder)

        _ ->
            Decode.fail "Invalid question type"


answerDecoder : Decoder String
answerDecoder =
    Decode.field "answer" Decode.string


radioAnswerFromSymbol : String -> Decoder RadioAnswer
radioAnswerFromSymbol str =
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


init : Value -> Model
init v =
    { exercises = mkIDs <| Result.withDefault [] (decodeValue exercisesDecoder v), apply = Nothing, submitted = False }


type Msg
    = SetApplying RadioAnswer
    | OnRadioAnswer ID RadioAnswer
    | OnWriteAnswer ID InputType
    | OnSubmit
    | OnRestart


updateExercise exercises exerciseID fn =
    List.map
        (\x ->
            if x.id == exerciseID then
                { x | questions = fn x.questions }

            else
                x
        )
        exercises


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetApplying a ->
            ( { model | apply = Just a }, Cmd.none )

        OnRadioAnswer (ID exerciseID questionID) newAnswer ->
            let
                updateAnswer qTypes =
                    List.indexedMap
                        (\qID qt ->
                            case qt of
                                RadioQuestion radio ->
                                    if qID == questionID then
                                        setRadioAnswer radio (Just newAnswer)

                                    else
                                        qt

                                _ ->
                                    qt
                        )
                        qTypes
            in
            ( { model | exercises = updateExercise model.exercises exerciseID updateAnswer }, Cmd.none )

        OnWriteAnswer (ID exerciseID questionID) newAnswer ->
            let
                updateAnswer qTypes =
                    List.indexedMap
                        (\qID qt ->
                            case qt of
                                WriteQuestion write ->
                                    if qID == questionID then
                                        setWriteAnswer write (Just newAnswer)

                                    else
                                        qt

                                _ ->
                                    qt
                        )
                        qTypes
            in
            ( { model | exercises = updateExercise model.exercises exerciseID updateAnswer }, Cmd.none )

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
    tr [] (viewXText x.text :: viewQuestions x)


viewXText : String -> Html Msg
viewXText s =
    td [] [ span [] [ text s ] ]


viewQuestions : Exercise -> List (Html Msg)
viewQuestions x =
    List.indexedMap (\i a -> viewQuestion (ID x.id i) a) x.questions


viewQuestion : ID -> QuestionType -> Html Msg
viewQuestion id q =
    let
        go =
            case q of
                RadioQuestion radio ->
                    viewRadio id radio

                WriteQuestion write ->
                    viewWrite id write
    in
    td [] [ go ]


viewRadio : ID -> Radio -> Html Msg
viewRadio id (Radio ans yourAns) =
    let
        mkRadioGroup : List RadioValue -> Html Msg
        mkRadioGroup =
            div [] << List.map (mkRadio id (idToString id) yourAns)
    in
    case ans of
        WordType _ ->
            mkRadioGroup getWordTypes

        GrammarType _ ->
            mkRadioGroup getGrammarTypes


mkRadio : ID -> RadioName -> YourRadioAnswer -> RadioValue -> Html Msg
mkRadio id rName selected val =
    label [] [ input [ type_ "radio", name rName, checked (selected == Just val), onClick (OnRadioAnswer id val) ] [], text (toSymbol val) ]


viewWrite : ID -> Write -> Html Msg
viewWrite id (Write ans yourAns) =
    input [ type_ "text", onInput (OnWriteAnswer id << InputType) ] []



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


getWordTypes : List RadioAnswer
getWordTypes =
    List.map WordType [ Ism, Fil, Harf ]


getGrammarTypes : List RadioAnswer
getGrammarTypes =
    List.map GrammarType [ Rafa, Nasb, Jar ]


idToString : ID -> String
idToString (ID i j) =
    fromInt i ++ "-" ++ fromInt j


answerToString : RadioAnswer -> String
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


answerSymbol : YourRadioAnswer -> String
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


toSymbol : RadioAnswer -> String
toSymbol =
    answerSymbol << Just
