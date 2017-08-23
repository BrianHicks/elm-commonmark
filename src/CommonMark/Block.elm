module CommonMark.Block exposing (Block(..), parseBlockStructure)

import Parser exposing (..)
import Regex exposing (Regex)


-- regexes


endsWithNewline : Regex
endsWithNewline =
    Regex.regex "\\n$"


unescape : Char -> String -> String
unescape char =
    let
        regex =
            char
                |> String.fromChar
                |> (++) "\\"
                |> Regex.escape
                |> Regex.regex

        replacement =
            String.fromChar char
    in
    Regex.replace
        Regex.All
        regex
        (\_ -> replacement)



-- blocks


type HeadingKind
    = ATX
    | Setext


type Block
    = ThematicBreak
    | Heading HeadingKind Int (Maybe String)
    | Paragraph String
    | IndentedCodeBlock String
      -- wait, what? Why Does a blank line have a string attached? The answer:
      -- lines consisting only of whitespace.
    | BlankLine String


finalize : Block -> Block
finalize block =
    case block of
        IndentedCodeBlock code ->
            code
                |> Regex.replace (Regex.AtMost 1) endsWithNewline (always "")
                |> IndentedCodeBlock

        _ ->
            block


{-| Combine an already-parsed and new node, and return the combination of the
two (or if the node was closed, the new open node and the old closed one.)
-}
extend : Block -> List Block -> List Block
extend new existing =
    case ( new, existing ) of
        {-
           paragraphs
           ----------
        -}
        -- add a new line onto a paragraph
        ( Paragraph moreContent, (Paragraph content) :: rest ) ->
            (Paragraph <| content ++ "\n" ++ moreContent) :: rest

        {-
           setext headings
           ---------------
        -}
        -- combine a single-line paragraph with a setext heading
        ( Heading Setext level Nothing, (Paragraph content) :: rest ) ->
            Heading Setext level (Just content) :: rest

        -- convert a setext heading to a plain paragraph otherwise
        ( Heading Setext level (Just content), rest ) ->
            Paragraph content :: rest

        {-
           indented code blocks
           --------------------
        -}
        -- an indented code block following a paragraph is just a hanging indent
        ( IndentedCodeBlock notCode, (Paragraph content) :: rest ) ->
            (Paragraph <| content ++ "\n" ++ notCode) :: rest

        -- add a new line onto an indented code block
        ( IndentedCodeBlock moreCode, (IndentedCodeBlock code) :: rest ) ->
            (IndentedCodeBlock <| code ++ "\n" ++ moreCode) :: rest

        -- line breaks don't close indented blocks
        ( BlankLine _, (IndentedCodeBlock code) :: rest ) ->
            (IndentedCodeBlock <| code ++ "\n") :: rest

        -- ( _, (IndentedCodeBlock code) :: rest ) ->
        --     Debug.crash (toString ( toString new, existing ))
        {-
           everything else
           ---------------
        -}
        ( new, rest ) ->
            new :: rest


parseLine : String -> List Block -> Result Error (List Block)
parseLine line document =
    let
        possibilities : Parser Block
        possibilities =
            oneOf
                [ case List.head document of
                    Just (Paragraph _) ->
                        setextHeading

                    _ ->
                        fail "setext headings can only follow open paragraphs"

                -- and now the things that aren't dependent on the preceding content...
                , atxHeading
                , thematicBreak
                , indentedCodeBlock
                , blankLine
                , succeed <| Paragraph <| String.trim line
                ]
    in
    run possibilities line
        |> Result.map (flip extend document)


parseBlockStructure : List String -> Result Error (List Block)
parseBlockStructure lines =
    let
        parseNextLine : ( Int, String ) -> Result Error (List Block) -> Result Error (List Block)
        parseNextLine ( rowNumber, line ) progressOrError =
            case Result.andThen (parseLine line) progressOrError of
                Ok newDocument ->
                    Ok newDocument

                -- change the line number so we don't lose those in error messages
                Err err ->
                    Err { err | row = rowNumber }
    in
    lines
        |> List.indexedMap (\rowNumber line -> ( rowNumber + 1, line ))
        |> List.foldl parseNextLine (Ok [])
        |> Result.map (List.map finalize)
        |> Result.map List.reverse



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


blankLine : Parser Block
blankLine =
    inContext "a blank line" <|
        neverCommit
            (succeed BlankLine
                |= keep zeroOrMore whitespace
                |. end
            )


neverCommit : Parser a -> Parser a
neverCommit parser =
    delayedCommitMap (\a _ -> a) parser (succeed ())


thematicBreak : Parser Block
thematicBreak =
    let
        single : Char -> Parser ()
        single breakChar =
            ignore (Exactly 1) ((==) breakChar)
                |. ignore zeroOrMore whitespace

        lineOf : Char -> Parser ()
        lineOf breakChar =
            repeat (AtLeast 3) (single breakChar) |> map (always ())
    in
    inContext "thematic break" <|
        succeed ThematicBreak
            |. neverCommit
                (oneToThreeSpaces
                    |. oneOf
                        [ lineOf '*'
                        , lineOf '-'
                        , lineOf '_'
                        ]
                    |. end
                )


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

        words : Parser String
        words =
            succeed
                (String.concat
                    >> String.trim
                    >> unescape '#'
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
        delayedCommitMap (Heading ATX)
            (succeed identity
                |. oneToThreeSpaces
                |= level
            )
            (oneOf
                [ succeed identity
                    |. ignore oneOrMore ((==) ' ')
                    |= words
                , succeed ""
                ]
                |. end
                |> map
                    (\stuff ->
                        if stuff == "" then
                            Nothing
                        else
                            Just stuff
                    )
            )


{-| since we're parsing line-by-line, setext headings just need to find the
underline character, but produce a heading. So we use an empty Heading for that
purpose.
-}
setextHeading : Parser Block
setextHeading =
    let
        level : Parser Int
        level =
            oneOf
                [ succeed 1
                    |. ignore oneOrMore ((==) '=')
                , succeed 2
                    |. ignore oneOrMore ((==) '-')
                ]
    in
    inContext "in a potential setext heading" <|
        delayedCommitMap (Heading Setext)
            (succeed identity
                |. oneToThreeSpaces
                |= level
                |. ignore zeroOrMore whitespace
                |. end
            )
            (succeed Nothing)


indentedCodeBlock : Parser Block
indentedCodeBlock =
    inContext "in an indented code block" <|
        neverCommit <|
            succeed IndentedCodeBlock
                |. ignore (Exactly 4) whitespace
                |= keep oneOrMore (\_ -> True)
                |. end
