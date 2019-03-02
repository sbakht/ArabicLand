module Ch1 exposing (Model, Msg, init, update, view)

import Element exposing (Attribute, Element, centerX, column, el, fill, html, htmlAttribute, layout, none, paragraph, rgb, row, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html, a, div, li, span, ul)
import Html.Attributes exposing (class, classList)
import Html.Events
import Json.Decode as Decode exposing (Decoder, Value, decodeValue, succeed)
import List exposing (concat)
import String exposing (fromInt)


type alias Model =
    { questions : List Sentence, apply : Maybe Answer, submitted : Bool }


type Answer
    = WordType WordType
    | GrammarType GrammarType


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


questionDecoder : Decoder (List (List Word))
questionDecoder =
    let
        mapper : String -> Maybe Answer -> Word
        mapper s ma =
            case ma of
                Just a ->
                    Question s a Nothing

                Nothing ->
                    Text s
    in
    Decode.list
        (Decode.list <|
            Decode.map2 mapper
                (Decode.field "word" Decode.string)
                (Decode.maybe <| Decode.field "answer" answerDecoder)
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
    { questions = Result.withDefault [] (decodeValue questionDecoder v), apply = Nothing, submitted = False }


type Msg
    = SetApplying Answer
    | OnAnswer Word YourAnswer
    | OnSubmit
    | OnRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetApplying a ->
            ( { model | apply = Just a }, Cmd.none )

        OnAnswer question answer ->
            let
                updateAnswers =
                    List.map
                        (List.map
                            (\q ->
                                if q == question then
                                    setAnswer q answer

                                else
                                    q
                            )
                        )
                        model.questions
            in
            ( { model | questions = updateAnswers }, Cmd.none )

        OnSubmit ->
            ( { model | submitted = True }, Cmd.none )

        OnRestart ->
            let
                resetAnswers =
                    List.map (List.map clearAnswer) model.questions
            in
            ( { model | questions = resetAnswers, submitted = False, apply = Nothing }, Cmd.none )



---------------------------------------------------


view : Model -> Html Msg
view model =
    layout []
        (column [ width fill ]
            (if model.submitted then
                answerState model

             else
                quizState model
            )
        )


quizState : Model -> List (Element Msg)
quizState model =
    [ viewQuestions model.submitted model.questions
    , el [ centerX ] <| viewSubmit
    ]


answerState : Model -> List (Element Msg)
answerState model =
    [ viewQuestions model.submitted model.questions
    , el [ centerX ] <| viewResults model.questions
    ]



---------------------------------------------------


viewQuestions : Submitted -> List Sentence -> Element Msg
viewQuestions submitted questions =
    questions
        |> List.map (viewChipGroup submitted)
        |> div []
        |> html


viewChipGroup : Submitted -> Sentence -> Html Msg
viewChipGroup submitted q =
    div [ class "pure-menu-horizontal" ]
        [ ul [] (List.map (selectView submitted) q)
        ]


selectView : Submitted -> Word -> Html Msg
selectView submitted q =
    case q of
        Question str a a1 ->
            viewChipWithDropdown submitted q

        Text str ->
            span [ class "pure-menu-item no-chip" ] [ Html.text str ]


viewChipWithDropdown : Submitted -> Word -> Html Msg
viewChipWithDropdown submitted q =
    div [ class "pure-menu-item pure-menu-allow-hover" ]
        [ span [ class "question-chip" ] [ viewClickableChip submitted q ]
        , ul [ class "pure-menu-children" ] (dropdownChoices q)
        ]


viewClickableChip : Submitted -> Word -> Html Msg
viewClickableChip submitted q =
    case q of
        Question str a a1 ->
            let
                icon =
                    answerSymbol a1

                isIncorrect =
                    Just a /= a1
            in
            div [ class "md-chips" ]
                [ div [ class "md-chip md-chip-hover md-chip-clickable" ]
                    [ div [ class "md-chip-icon", classList [ ( "icon-unanswered", answerSymbol a1 == "?" ), ( "icon-wrong", submitted && isIncorrect ) ] ] [ Html.text icon ]
                    , Html.text str
                    ]
                ]

        Text _ ->
            Html.text ""



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


dropdownChoices : Word -> List (Html Msg)
dropdownChoices q =
    case q of
        Question _ a _ ->
            case a of
                WordType _ ->
                    List.map (dropdownItem q) [ WordType Ism, WordType Fil, WordType Harf ]

                GrammarType _ ->
                    List.map (dropdownItem q) [ GrammarType Rafa, GrammarType Nasb, GrammarType Jar ]

        Text _ ->
            []


dropdownItem : Word -> Answer -> Html Msg
dropdownItem q ans =
    li [ class "pure-menu-item" ] [ a [ Html.Events.onClick (OnAnswer q (Just ans)), class "pure-menu-link" ] [ Html.text (answerToString ans) ] ]


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
