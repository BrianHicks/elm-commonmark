module CommonMark exposing (Block(..), Document, parseString)

{-| Parse [CommonMark](http://commonmark.org/)-formatted Markdown files.

@docs parseString


## Block-level Elements

@docs Document, Block

-}

import Parser exposing (..)
import Parser.LowLevel as LowLevel
import String.Extra


{-| a document is just a list of Block-level elements
-}
type alias Document =
    List Block


{-| Markdown is basically a list of block-level elements which all contain one
or more block- or inline-level elements.
-}
type Block
    = ThematicBreak
    | Heading Int String


whitespace : Char -> Bool
whitespace c =
    c == ' ' || c == '\t'


newlineChar : Char -> Bool
newlineChar c =
    c == '\n' || c == '\x0D'


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
        single : Char -> Parser ()
        single breakChar =
            ignore (Exactly 1) ((==) breakChar)
                |. ignore zeroOrMore whitespace

        lineOf : Char -> Parser ()
        lineOf breakChar =
            delayedCommit
                (oneToThreeSpaces |. single breakChar)
                (repeat (AtLeast 2) (single breakChar) |> map (always ()))
    in
    inContext "thematic break" <|
        succeed ThematicBreak
            |. oneOf
                [ lineOf '*'
                , lineOf '-'
                , lineOf '_'
                ]
            |. eol


atxHeading : Parser Block
atxHeading =
    let
        level : Parser Int
        level =
            let
                count =
                    keep (AtLeast 1) ((==) '#')
                        |> map String.length

                validate n =
                    if n <= 6 then
                        succeed n
                    else
                        fail <| "cannot have a header level higher than 6, got " ++ toString n
            in
            count |> andThen validate

        trailers : Parser String
        trailers =
            oneOf
                [ -- the closing header of `## foo ##`
                  delayedCommit
                    (ignore oneOrMore whitespace
                        |. ignore oneOrMore ((==) '#')
                        |. ignore zeroOrMore whitespace
                    )
                    (oneOf
                        [ peek "\n"
                        , peek "\x0D\n"
                        , end
                        ]
                    )
                    |> map (\_ -> "")

                -- the trailing whitespace of `## foo  `
                , ignore zeroOrMore whitespace
                    |> source
                ]

        wordsWithoutClosingSequence : Parser String
        wordsWithoutClosingSequence =
            map2 (++)
                (keep oneOrMore
                    (\c -> not (whitespace c) && not (newlineChar c))
                    |> map (String.Extra.replace "\\#" "#")
                )
                trailers
                |> repeat oneOrMore
                |> map (String.concat >> String.trim)
                |> inContext "in the header contents"
    in
    inContext "in an ATX heading" <|
        delayedCommitMap Heading
            (succeed identity
                |. oneToThreeSpaces
                |= level
            )
            (succeed identity
                |. ignore oneOrMore ((==) ' ')
                |= wordsWithoutClosingSequence
                |. eol
            )


{-| parse a string into CommonMark AST. You can format this however you like;
it's just a list of block-level elements.

TODO: examples! And elm-doc-test!

-}
parseString : String -> Result Error Document
parseString raw =
    let
        root =
            oneOf
                [ atxHeading
                , thematicBreak
                ]
                |> repeat zeroOrMore
                |> inContext "a Markdown document"
    in
    run root raw



-- UTILS


peek : String -> Parser ()
peek stuff =
    succeed (,)
        |= LowLevel.getOffset
        |= LowLevel.getSource
        |> andThen
            (\( pos, src ) ->
                if String.slice pos (pos + String.length stuff) src == stuff then
                    succeed ()
                else
                    fail "peek failed"
            )
