module Ch1 exposing (Answer(..), Model, Msg(..), Question(..), YourAnswer, black, blue, class, classList, clearAnswer, clickState, color, green, init, isCorrect, isIncorrect, isNotAnswered, numCorrect, numQuestions, red, setAnswer, test, update, view, viewAnswerKey, viewApply, viewButtons, viewQuestion, viewQuestionAnswer, viewQuestions, viewRestart, viewResultStatus, viewSubmit)

import Element exposing (Attribute, Element, column, el, htmlAttribute, layout, none, paragraph, rgb, row, text)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
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
    | OnAnswer Question
    | OnSubmit
    | OnRestart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetApplying a ->
            ( { model | apply = Just a }, Cmd.none )

        OnAnswer question ->
            let
                updateAnswers =
                    List.map
                        (\q ->
                            if q == question then
                                setAnswer q model.apply

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
    layout [] (column [] [ viewApply model.apply, viewQuestions model.questions viewQuestion, viewButtons model ])


viewQuestions : List Question -> (Question -> Element Msg) -> Element Msg
viewQuestions questions fn =
    paragraph [] <| List.intersperse (text " ") <| List.map fn questions


viewQuestion : Question -> Element Msg
viewQuestion ((Question str _ a) as q) =
    el [ onClick (OnAnswer q), color a ] (text str)


viewAnswerKey : List Question -> (Question -> Element Msg) -> Element Msg
viewAnswerKey qs fn =
    if numCorrect qs < numQuestions qs then
        column [] [ text "Answer Key", viewQuestions qs fn ]

    else
        none


viewQuestionAnswer : Question -> Element Msg
viewQuestionAnswer (Question str a _) =
    el [ color (Just a) ] (text str)


viewApply : YourAnswer -> Element Msg
viewApply ans =
    row []
        [ clickState "Ism" Ism ans
        , clickState "Fil" Fil ans
        , clickState "Harf" Harf ans
        ]


clickState : String -> Answer -> YourAnswer -> Element Msg
clickState label answer yourAns =
    el [ onClick (SetApplying answer), classList [ ( "apply-selected", Just answer == yourAns ) ], color (Just answer) ] (text label)


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
    button []
        { onPress = Just OnSubmit
        , label = text "Check Answers"
        }


viewRestart : Element Msg
viewRestart =
    button []
        { onPress = Just OnRestart
        , label = text "Start Over"
        }


class s =
    htmlAttribute (Html.Attributes.class s)


classList s =
    htmlAttribute (Html.Attributes.classList s)


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
