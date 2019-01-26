module Word exposing (Word, mkImplicitWord, mkWord, wordToString)


type Word
    = NormalWord { id : Int, text : String }
    | HiddenWord { id : Int, text : String }
    | ConnectorWord { id : Int, text : String }


mkWord : Int -> String -> Word
mkWord id text =
    NormalWord { id = id, text = text }


mkImplicitWord : Int -> String -> Word
mkImplicitWord id text =
    HiddenWord { id = id, text = text }


wordToString : Word -> String
wordToString word =
    case word of
        NormalWord x ->
            x.text

        HiddenWord x ->
            "HIDDEN"

        ConnectorWord _ ->
            "CONNECTOR"
