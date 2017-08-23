module CommonMark.Block exposing (Block(..), parseBlockStructure)

import Parser exposing (..)
import Regex exposing (Regex)
import String.Extra


-- regexes


endsWithNewline : Regex
endsWithNewline =
    Regex.regex "\\n$"



-- blocks


type Block
    = ThematicBreak
    | Heading Int (Maybe String)
    | Paragraph String
    | IndentedCodeBlock String
      -- wait, what? Why Does a blank line have a string attached? The answer:
      -- lines consisting only of whitespace.
    | BlankLine String


type Status
    = Open
    | Closed


type Node
    = Node Status Block


finalize : Node -> Block
finalize (Node _ block) =
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
extend : Maybe Node -> Node -> List Node
extend existing new =
    case ( existing, new ) of
        {-
           paragraphs
           ----------
        -}
        -- add a new line onto a paragraph
        ( Just (Node Open (Paragraph content)), Node Open (Paragraph moreContent) ) ->
            [ Node Open (Paragraph (content ++ "\n" ++ moreContent)) ]

        {-
           setext headings
           ---------------
        -}
        -- combine a single-line paragraph with a setext heading
        ( Just (Node Open (Paragraph content)), Node Open (Heading level _) ) ->
            [ Node Open (Heading level (Just content)) ]

        -- convert a setext heading to a plain paragraph otherwise
        ( _, Node Open (Heading level (Just content)) ) ->
            [ Node Open (Paragraph content) ]

        {-
           indented code blocks
           --------------------
        -}
        -- an indented code block following a paragraph is just a hanging indent
        ( Just (Node Open (Paragraph content)), Node Open (IndentedCodeBlock notCode) ) ->
            [ Node Open (Paragraph <| content ++ "\n" ++ notCode) ]

        -- add a new line onto an indented code block
        ( Just (Node Open (IndentedCodeBlock code)), Node Open (IndentedCodeBlock moreCode) ) ->
            [ Node Open (IndentedCodeBlock <| code ++ "\n" ++ moreCode) ]

        -- line breaks don't close indented blocks
        ( Just (Node Open (IndentedCodeBlock code)), Node Closed (BlankLine _) ) ->
            [ Node Open (IndentedCodeBlock <| code ++ "\n") ]

        {-
           blank lines
           -----------
        -}
        -- blank lines breaks close everything else
        ( Just (Node Open block), Node Closed (BlankLine _) ) ->
            [ Node Closed block ]

        {-
           everything else
           ---------------
        -}
        ( Just anythingElse, _ ) ->
            [ new, anythingElse ]

        ( Nothing, _ ) ->
            [ new ]


parseBlockStructure : List String -> Result Error (List Block)
parseBlockStructure lines =
    let
        parseNextLine : ( Int, String ) -> Result Error (List Node) -> Result Error (List Node)
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


parseLine : String -> List Node -> Result Error (List Node)
parseLine line document =
    let
        possibilities : Parser Node
        possibilities =
            oneOf
                [ case List.head document of
                    Just (Node Open (Paragraph _)) ->
                        setextHeading

                    _ ->
                        fail "setext headings can only follow open paragraphs"

                -- and now the things that aren't dependent on the preceding content...
                , atxHeading
                , thematicBreak
                , indentedCodeBlock
                , blankLine
                , succeed <| Node Open <| Paragraph <| String.trim line
                ]

        closeOrExtendHead : List Node -> Node -> List Node
        closeOrExtendHead document node =
            case document of
                [] ->
                    extend Nothing node

                first :: rest ->
                    extend (Just first) node ++ rest
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


blankLine : Parser Node
blankLine =
    inContext "a blank line" <|
        neverCommit
            (succeed (Node Closed << BlankLine)
                |= keep zeroOrMore whitespace
                |. end
            )


neverCommit : Parser a -> Parser a
neverCommit parser =
    delayedCommitMap (\a _ -> a) parser (succeed ())


thematicBreak : Parser Node
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
        succeed (Node Closed ThematicBreak)
            |. neverCommit
                (oneToThreeSpaces
                    |. oneOf
                        [ lineOf '*'
                        , lineOf '-'
                        , lineOf '_'
                        ]
                    |. end
                )


atxHeading : Parser Node
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
                    >> String.Extra.replace "\\#" "#"
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
        map (Node Closed) <|
            delayedCommitMap Heading
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
setextHeading : Parser Node
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
        map (Node Open) <|
            delayedCommitMap Heading
                (succeed identity
                    |. oneToThreeSpaces
                    |= level
                    |. ignore zeroOrMore whitespace
                    |. end
                )
                (succeed Nothing)


indentedCodeBlock : Parser Node
indentedCodeBlock =
    inContext "in an indented code block" <|
        neverCommit <|
            succeed (Node Open << IndentedCodeBlock)
                |. ignore (Exactly 4) whitespace
                |= keep oneOrMore (\_ -> True)
                |. end
