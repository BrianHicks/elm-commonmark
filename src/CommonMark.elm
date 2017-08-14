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
        [ keyword "   "
        , keyword "  "
        , keyword " "
        , succeed ()
        ]


eol : Parser ()
eol =
    oneOf
        [ keyword "\n"
        , keyword "\x0D\n"
        , end
        ]


thematicBreak : Parser Block
thematicBreak =
    let
        anyBreakChar : Char -> Bool
        anyBreakChar c =
            c == '*' || c == '-' || c == '_'

        initial : Parser Char
        initial =
            succeed identity
                |= (keep (Exactly 1) anyBreakChar
                        |> map
                            (String.toList
                                >> List.head
                                -- we already know we have `Just x` so this is
                                -- just to break out of the `Maybe`. That said,
                                -- if this ever *does* happen we at least want
                                -- an indication of what went wrong. So,
                                -- frowning face!
                                --
                                -- https://github.com/elm-tools/parser/issues/19
                                >> Maybe.withDefault '🙍'
                            )
                   )
                |. ignore zeroOrMore whitespace

        subsequent : Char -> Parser ()
        subsequent c =
            ignore (Exactly 1) ((==) c) |. ignore zeroOrMore whitespace
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
