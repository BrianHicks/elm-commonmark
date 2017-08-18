module CommonMark exposing (Block(..), Document, Inline(..), parseString)

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
      -- TODO: Should Heading have `Maybe Inline` instead for no-content headings?
    | Heading Int Inline
    | Paragraph Inline


type Inline
    = Plain String


parseString : String -> Result Error Document
parseString raw =
    raw
        |> String.lines
        |> parseBlockStructure


parseBlockStructure : List String -> Result Error Document
parseBlockStructure lines =
    let
        helper : ( Int, String ) -> Result Error Document -> Result Error Document
        helper ( rowNumber, line ) progressOrError =
            case Result.andThen (parseBlockLine line) progressOrError of
                Ok newDocument ->
                    Ok newDocument

                Err err ->
                    Err { err | row = rowNumber }
    in
    -- TODO: close terminal partial block and reverse list
    lines
        |> List.indexedMap (\rowNumber line -> ( rowNumber + 1, line ))
        |> List.foldl helper (Ok [])
        |> Result.map List.reverse


parseBlockLine : String -> Document -> Result Error Document
parseBlockLine line document =
    let
        possibilities : Parser Block
        possibilities =
            oneOf
                [ thematicBreak
                , atxHeading
                , succeed <| Paragraph <| Plain line
                ]

        closeOrExtendHead : Document -> Block -> Document
        closeOrExtendHead document block =
            case ( document, block ) of
                ( [], _ ) ->
                    [ block ]

                ( anythingElse, _ ) ->
                    block :: anythingElse
    in
    run possibilities line
        |> Result.map (closeOrExtendHead document)



-- parsers!


whitespace : Char -> Bool
whitespace c =
    c == ' ' || c == '\t'


oneToThreeSpaces : Parser ()
oneToThreeSpaces =
    oneOf
        [ symbol "   "
        , symbol "  "
        , symbol " "
        , succeed ()
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
                (repeat (AtLeast 3) (single breakChar) |> map (always ()))
                (succeed ())
    in
    inContext "thematic break" <|
        succeed ThematicBreak
            |. delayedCommit
                (oneToThreeSpaces
                    |. oneOf
                        [ lineOf '*'
                        , lineOf '-'
                        , lineOf '_'
                        ]
                    |. end
                )
                (succeed ())


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

        words : Parser Inline
        words =
            succeed
                (String.concat
                    >> String.trim
                    >> String.Extra.replace "\\#" "#"
                    >> Plain
                )
                |= (repeat zeroOrMore <|
                        oneOf
                            [ -- closing hashes like in `## foo ##`
                              delayedCommit
                                (ignore oneOrMore ((==) '#')
                                    |. ignore zeroOrMore whitespace
                                    |. end
                                )
                                (succeed "")

                            -- normal non-whitespace non-closing header word characters
                            , keep oneOrMore (not << whitespace)

                            -- regular single spaces
                            , keep (Exactly 1) ((==) ' ')
                            ]
                   )
    in
    inContext "in an ATX heading" <|
        delayedCommitMap Heading
            (succeed identity
                |. oneToThreeSpaces
                |= level
            )
            (oneOf
                [ succeed identity
                    |. ignore oneOrMore ((==) ' ')
                    |= words
                , succeed (Plain "")
                ]
                |. end
            )
