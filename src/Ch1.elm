module Ch1 exposing (Answer(..), Model, Msg(..), Question(..), YourAnswer, black, blue, clearAnswer, clickState, color, green, init, isCorrect, isIncorrect, isNotAnswered, numCorrect, numQuestions, red, setAnswer, test, update, view, viewAnswerKey, viewButtons, viewQuestion, viewQuestionAnswer, viewQuestions, viewRestart, viewResultStatus, viewSubmit)

import Element exposing (Attribute, Element, centerX, column, el, fill, html, htmlAttribute, layout, none, paragraph, rgb, row, text, width)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html, a, div, li, span, ul)
import Html.Attributes exposing (class, classList)
import Html.Events
import String exposing (fromInt)


type alias Model =
    { questions : List Question, apply : Maybe Answer, submitted : Bool }


type Answer
    = Ism
    | Fil
    | Harf


type alias YourAnswer =
    Maybe Answer


type Question
    = Question String Answer YourAnswer


isCorrect : Question -> Bool
isCorrect (Question _ a a1) =
    Just a == a1


isIncorrect : Question -> Bool
isIncorrect =
    not << isCorrect


isNotAnswered : Question -> Bool
isNotAnswered (Question _ _ a1) =
    a1 == Nothing


setAnswer : Question -> YourAnswer -> Question
setAnswer (Question s a _) ans =
    Question s a ans


clearAnswer : Question -> Question
clearAnswer q =
    setAnswer q Nothing


numCorrect : List Question -> Int
numCorrect =
    List.length << List.filter (\x -> x) << List.map isCorrect


numQuestions : List Question -> Int
numQuestions =
    List.length


init : Model
init =
    { questions = test, apply = Nothing, submitted = False }


test =
    [ Question "We" Ism Nothing
    , Question "invited" Fil Nothing
    , Question "guests" Ism Nothing
    , Question "for" Harf Nothing
    , Question "dinner" Ism Nothing
    ]


type Msg
    = SetApplying Answer
    | OnAnswer Question YourAnswer
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
                        (\q ->
                            if q == question then
                                setAnswer q answer

                            else
                                q
                        )
                        model.questions
            in
            ( { model | questions = updateAnswers }, Cmd.none )

        OnSubmit ->
            ( { model | submitted = True }, Cmd.none )

        OnRestart ->
            let
                resetAnswers =
                    List.map clearAnswer model.questions
            in
            ( { model | questions = resetAnswers, submitted = False, apply = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    layout [] (column [width fill] [ viewQuestions model.questions viewQuestion, el [centerX] <| viewButtons model ])


viewQuestions : List Question -> (Question -> Html Msg) -> Element Msg
viewQuestions questions fn =
    html <| div [ class "pure-menu-horizontal"] <| [ul [] (List.map fn questions)]


viewQuestion : Question -> Html Msg
viewQuestion ((Question str _ a) as q) =
    div [ class "pure-menu-item pure-menu-allow-hover"] [
        span [ class "question-chip" ] [chip (answerSymbol a) str],
        ul [ class "pure-menu-children"] (dropdownChoices q)
    ]

chip : String -> String -> Html Msg
chip icon str = div [ class "md-chips"] [
        div [ class "md-chip md-chip-hover md-chip-clickable"] [
                div [ class "md-chip-icon", classList [("icon-unanswered", icon == "?")]] [Html.text icon],
                Html.text str
            ]
    ]

dropdownChoices : Question -> List (Html Msg)
dropdownChoices q = List.map (dropdownItem q) [Ism, Fil, Harf]

dropdownItem: Question -> Answer -> Html Msg
dropdownItem q ans = li [class "pure-menu-item"] [a [Html.Events.onClick (OnAnswer q (Just ans)) , class "pure-menu-link"] [Html.text (answerToString ans)]]


viewAnswerKey : List Question -> (Question -> Element Msg) -> Element Msg
viewAnswerKey qs fn =
    if numCorrect qs < numQuestions qs then
--        column [] [ text "Answer Key", viewQuestions qs fn ]
        none

    else
        none


viewQuestionAnswer : Question -> Element Msg
viewQuestionAnswer (Question str a _) =
    el [ color (Just a) ] (text str)


answerToString : Answer -> String
answerToString answer = case answer of
    Ism ->
        "Ism"
    Fil ->
        "Fil"
    Harf ->
        "Harf"

answerSymbol : YourAnswer -> String
answerSymbol a = case a of
    Just Ism ->
        "I"
    Just Fil ->
        "F"
    Just Harf ->
        "H"
    Nothing ->
        "?"


clickState : String -> Answer -> YourAnswer -> Element Msg
clickState label answer yourAns =
    el [ onClick (SetApplying answer), classListC [ ( "apply-selected", Just answer == yourAns ) ], color (Just answer) ] (text label)


viewButtons : Model -> Element Msg
viewButtons model =
    if model.submitted then
        column []
            [ viewAnswerKey model.questions viewQuestionAnswer
            , viewRestart
            , viewResultStatus model.questions
            ]

    else
        viewSubmit


viewResultStatus : List Question -> Element Msg
viewResultStatus qs =
    text ("You got " ++ fromInt (numCorrect qs) ++ "/" ++ fromInt (numQuestions qs) ++ " correct.")


viewSubmit : Element Msg
viewSubmit =
    html <| Html.button [Html.Events.onClick OnSubmit, class "pure-button pure-button-primary"] [Html.text "Check Answers"]


viewRestart : Element Msg
viewRestart =
    html <| Html.button [Html.Events.onClick OnRestart, class "pure-button button-warning"] [Html.text "Start Over"]


classC s =
    htmlAttribute (Html.Attributes.class s)


classListC s =
    htmlAttribute (classList s)


color : YourAnswer -> Attribute Msg
color a =
    case a of
        Just Ism ->
            green

        Just Fil ->
            red

        Just Harf ->
            blue

        Nothing ->
            black


green =
    Font.color <| rgb 0 255 0


red =
    Font.color <| rgb 255 0 0


blue =
    Font.color <| rgb 0 0 255


black =
    Font.color <| rgb 0 0 0
