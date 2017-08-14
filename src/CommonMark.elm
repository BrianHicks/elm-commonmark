module CommonMark exposing (..)

import Parser exposing (..)


type alias Document =
    List Block


type Block
    = ThematicBreak
    | TODO


whitespace : Char -> Bool
whitespace c =
    c == ' ' || c == '\t'


{-| Most of the CommonMark block elements allow one to three spaces before the
beginning of the block. So, helper parser!
-}
oneToThreeSpaces : Parser ()
oneToThreeSpaces =
    oneOf
        [ symbol "   "
        , symbol "  "
        , symbol " "
        , succeed ()
        ]


eol : Parser ()
eol =
    oneOf
        [ symbol "\n"
        , symbol "\x0D\n"
        , end
        ]


thematicBreak : Parser Block
thematicBreak =
    let
        anyBreakChar : Char -> Bool
        anyBreakChar c =
            c == '*' || c == '-' || c == '_'

        initial : Parser String
        initial =
            succeed identity
                |= keep (Exactly 1) anyBreakChar
                |. ignore zeroOrMore whitespace

        subsequent : String -> Parser ()
        subsequent breakChar =
            symbol breakChar |. ignore zeroOrMore whitespace
    in
    inContext "thematic break"
        (succeed ThematicBreak
            |. oneToThreeSpaces
            |. (initial |> andThen (\c -> repeat (AtLeast 2) (subsequent c)))
            |. eol
        )


parseString : String -> Result Error Document
parseString raw =
    run thematicBreak raw
        |> Result.map List.singleton
