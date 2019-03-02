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
    { questions : List Questions, apply : Maybe Answer, submitted : Bool }


type Answer
    = Ism
    | Fil
    | Harf


type alias YourAnswer =
    Maybe Answer


type alias Submitted =
    Bool


type alias Questions =
    List Question


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


numCorrect : List Questions -> Int
numCorrect =
    List.length << List.filter (\x -> x) << List.map isCorrect << concat


numQuestions : List Questions -> Int
numQuestions =
    List.length << concat


questionDecoder : Decoder (List (List Question))
questionDecoder =
    Decode.list
        (Decode.list <|
            Decode.map2 (\s a -> Question s a Nothing)
                (Decode.field "word" Decode.string)
                (Decode.field "answer" answerDecoder)
        )


answerDecoder : Decoder Answer
answerDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "I" ->
                        Decode.succeed Ism

                    "F" ->
                        Decode.succeed Fil

                    "H" ->
                        Decode.succeed Harf

                    _ ->
                        Decode.fail "Invalid answer choice"
            )


init : Value -> Model
init v =
    { questions = Result.withDefault [] (decodeValue questionDecoder v), apply = Nothing, submitted = False }


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


viewQuestions : Submitted -> List Questions -> Element Msg
viewQuestions submitted questions =
    questions
        |> List.map (viewChipGroup submitted)
        |> div []
        |> html


viewChipGroup : Submitted -> Questions -> Html Msg
viewChipGroup submitted q =
    div [ class "pure-menu-horizontal" ]
        [ ul [] (List.map (viewChipWithDropdown submitted) q)
        ]


viewChipWithDropdown : Submitted -> Question -> Html Msg
viewChipWithDropdown submitted q =
    div [ class "pure-menu-item pure-menu-allow-hover" ]
        [ span [ class "question-chip" ] [ viewClickableChip submitted q ]
        , ul [ class "pure-menu-children" ] (dropdownChoices q)
        ]


viewClickableChip : Submitted -> Question -> Html Msg
viewClickableChip submitted ((Question str _ a) as q) =
    let
        icon =
            answerSymbol a
    in
    div [ class "md-chips" ]
        [ div [ class "md-chip md-chip-hover md-chip-clickable" ]
            [ div [ class "md-chip-icon", classList [ ( "icon-unanswered", answerSymbol a == "?" ), ( "icon-wrong", submitted && isIncorrect q ) ] ] [ Html.text icon ]
            , Html.text str
            ]
        ]



---------------------------------------------------


viewResults : List Questions -> Element Msg
viewResults questions =
    column []
        [ viewResultStatus questions
        , viewRestart
        ]


viewResultStatus : List Questions -> Element Msg
viewResultStatus qs =
    text ("You got " ++ fromInt (numCorrect qs) ++ "/" ++ fromInt (numQuestions qs) ++ " correct.")


viewSubmit : Element Msg
viewSubmit =
    html <| Html.button [ Html.Events.onClick OnSubmit, class "pure-button pure-button-primary" ] [ Html.text "Check Answers" ]


viewRestart : Element Msg
viewRestart =
    html <| Html.button [ Html.Events.onClick OnRestart, class "pure-button button-warning" ] [ Html.text "Start Over" ]



---------------------------------------------------


dropdownChoices : Question -> List (Html Msg)
dropdownChoices q =
    List.map (dropdownItem q) [ Ism, Fil, Harf ]


dropdownItem : Question -> Answer -> Html Msg
dropdownItem q ans =
    li [ class "pure-menu-item" ] [ a [ Html.Events.onClick (OnAnswer q (Just ans)), class "pure-menu-link" ] [ Html.text (answerToString ans) ] ]


answerToString : Answer -> String
answerToString answer =
    case answer of
        Ism ->
            "Ism"

        Fil ->
            "Fil"

        Harf ->
            "Harf"


answerSymbol : YourAnswer -> String
answerSymbol a =
    case a of
        Just Ism ->
            "I"

        Just Fil ->
            "F"

        Just Harf ->
            "H"

        Nothing ->
            "?"
