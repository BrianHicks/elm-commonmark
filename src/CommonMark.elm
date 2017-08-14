module CommonMark exposing (Block(..), Document, parseString)

{-| Parse [CommonMark](http://commonmark.org/)-formatted Markdown files.

@docs parseString


## Block-level Elements

@docs Document, Block

-}

import Parser exposing (..)


{-| a document is just a list of Block-level elements
-}
type alias Document =
    List Block


{-| Markdown is basically a list of block-level elements which all contain one
or more block- or inline-level elements.
-}
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


{-| parse a string into CommonMark AST. You can format this however you like;
it's just a list of block-level elements.

TODO: examples! And elm-doc-test!

-}
parseString : String -> Result Error Document
parseString raw =
    run thematicBreak raw
        |> Result.map List.singleton
