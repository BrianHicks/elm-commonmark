module CommonMark.Block exposing (Block(..), CodeBlockKind(..), parseBlockStructure)

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


type Status
    = Open
    | Closed


type HeadingKind
    = ATX
    | Setext


type CodeBlockKind
    = Indented
    | Fenced Status Int Int Char (Maybe String)


type Block
    = ThematicBreak
    | Heading HeadingKind Int (Maybe String)
    | Paragraph String
    | CodeBlockFence Int Int Char (Maybe String) String
    | CodeBlock CodeBlockKind (Maybe String)
      -- wait, what? Why Does a blank line have a string attached? The answer:
      -- lines consisting only of isWhitespace.
    | BlankLine String
      -- every other line, which will be interpreted based on the semantics of
      -- the blocks before and after it
    | Line String


finalize : Block -> Block
finalize block =
    case block of
        CodeBlock Indented code ->
            code
                |> Maybe.map (Regex.replace (Regex.AtMost 1) endsWithNewline (always ""))
                |> CodeBlock Indented

        CodeBlock (Fenced status indent count char info) (Just code) ->
            CodeBlock (Fenced status indent count char info) (Just <| code ++ "\n")

        _ ->
            block


{-| Combine an already-parsed and new node, and return the combination of the
two (or if the node was closed, the new open node and the old closed one.)
-}
extend : Block -> List Block -> List Block
extend new existing =
    case ( new, existing ) of
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
        ( CodeBlock Indented notCode, (Paragraph content) :: rest ) ->
            (Paragraph <| content ++ "\n" ++ Maybe.withDefault "" notCode) :: rest

        -- add a new line onto an indented code block
        ( CodeBlock Indented moreCode, (CodeBlock Indented code) :: rest ) ->
            (CodeBlock Indented <| joinMaybeLines moreCode code) :: rest

        -- line breaks don't close indented blocks
        ( BlankLine _, (CodeBlock Indented code) :: rest ) ->
            (CodeBlock Indented <| Maybe.map (flip (++) "\n") code) :: rest

        {-
           fenced code blocks
           ------------------
        -}
        ( CodeBlockFence _ length char info source, (CodeBlock (Fenced Open indent originalLength originalChar originalInfo) code) :: rest ) ->
            if char == originalChar && length >= originalLength && info == Nothing then
                CodeBlock
                    (Fenced Closed indent originalLength originalChar originalInfo)
                    code
                    :: rest
            else
                CodeBlock
                    (Fenced Open indent originalLength originalChar originalInfo)
                    (joinMaybeLines (Just source) code)
                    :: rest

        ( CodeBlockFence indent length char info source, rest ) ->
            CodeBlock (Fenced Open indent length char info) Nothing :: rest

        ( Line moreCode, (CodeBlock (Fenced Open indent length indicator info) code) :: rest ) ->
            CodeBlock
                (Fenced Open indent length indicator info)
                (joinMaybeLines
                    (Just <| trimLeftWhitespace indent moreCode)
                    code
                )
                :: rest

        -- line breaks don't close fenced code blocks
        ( BlankLine blanks, (CodeBlock (Fenced Open indent length indicator info) code) :: rest ) ->
            CodeBlock (Fenced Open indent length indicator info) (joinMaybeLines (Just blanks) code) :: rest

        {-
           paragraphs
           ----------
        -}
        -- add a new line onto a paragraph
        ( Line moreContent, (Paragraph content) :: rest ) ->
            (Paragraph <| content ++ "\n" ++ moreContent) :: rest

        {-
           everything else
           ---------------
        -}
        -- if we've made it this far without consuming the line, let's just call
        -- it a paragraph and move on with our lives.
        ( Line stuff, rest ) ->
            Paragraph (String.trim stuff) :: rest

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
                , case List.head document of
                    Just (CodeBlock (Fenced Open _ _ _ _) _) ->
                        fail "indented code blocks shouldn't be considered inside fenced code blocks"

                    _ ->
                        indentedCodeBlock

                -- and now the things that aren't dependent on the surrounding content...
                , atxHeading
                , thematicBreak
                , fencedCodeBlock
                , blankLine
                , succeed <| Line line
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


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t'


whitespaceExpandingTabs : Count -> Parser Int
whitespaceExpandingTabs count =
    let
        check : Int -> Parser Int
        check soFar =
            case count of
                AtLeast desired ->
                    if soFar >= desired then
                        succeed soFar
                    else
                        parseAndExpand soFar

                Exactly desired ->
                    if soFar == desired then
                        succeed soFar
                    else
                        parseAndExpand soFar

        tabstopSize =
            4

        {- Hitting space means "add one space", while hitting tab means "advance
           to the next tabstop".

           This means that:

           | Input           | Output |
           |=================|========|
           | space           | 1      |
           | space-space     | 2      |
           | space-tab       | 4      |
           | space-space-tab | 4      |
           | space-tab-space | 5      |
        -}
        parseAndExpand : Int -> Parser Int
        parseAndExpand start =
            oneOf
                [ ignore (Exactly 1) ((==) ' ')
                    |> map (\_ -> start + 1)
                    |> andThen check
                , ignore (Exactly 1) ((==) '\t')
                    |> map (\_ -> start + tabstopSize - start % tabstopSize)
                    |> andThen check
                ]
    in
    parseAndExpand 0


oneToThreeSpaces : Parser Int
oneToThreeSpaces =
    oneOf
        [ symbol "   " |> map (\_ -> 3)
        , symbol "  " |> map (\_ -> 2)
        , symbol " " |> map (\_ -> 1)
        , succeed 0
        ]


blankLine : Parser Block
blankLine =
    inContext "a blank line" <|
        neverCommit
            (succeed BlankLine
                |= keep zeroOrMore isWhitespace
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
                |. ignore zeroOrMore isWhitespace

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
                                    |. ignore zeroOrMore isWhitespace
                                    |. end
                                )
                                (succeed "")

                            -- normal non-isWhitespace non-closing header word characters
                            , keep oneOrMore (not << isWhitespace)

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
                    |. whitespaceExpandingTabs oneOrMore
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
                |. ignore zeroOrMore isWhitespace
                |. end
            )
            (succeed Nothing)


indentedCodeBlock : Parser Block
indentedCodeBlock =
    inContext "in an indented code block" <|
        neverCommit <|
            succeed (CodeBlock Indented << Just)
                |. whitespaceExpandingTabs (Exactly 4)
                |= keep oneOrMore (\_ -> True)
                |. end


fencedCodeBlock : Parser Block
fencedCodeBlock =
    let
        fenceIndicator : Char -> Parser ( Int, Char )
        fenceIndicator c =
            repeat
                (AtLeast 3)
                (ignore (Exactly 1) ((==) c))
                |> map (\units -> ( List.length units, c ))

        fence : Parser ( Int, Char )
        fence =
            oneOf
                [ fenceIndicator '`'
                , fenceIndicator '~'
                ]
    in
    inContext "in a fenced code block" <|
        neverCommit <|
            sourceMap
                (\source ( indent, ( size, char ), info ) ->
                    CodeBlockFence indent size char info source
                )
                (succeed (,,)
                    |= oneToThreeSpaces
                    |= fence
                    |. ignore zeroOrMore isWhitespace
                    |= oneOf
                        [ keep oneOrMore (not << isWhitespace) |> map Just
                        , succeed Nothing
                        ]
                    |. ignore zeroOrMore (\_ -> True)
                    |. end
                )


joinMaybeLines : Maybe String -> Maybe String -> Maybe String
joinMaybeLines down up =
    case ( down, up ) of
        ( Just d, Just u ) ->
            Just <| u ++ "\n" ++ d

        ( Just _, Nothing ) ->
            down

        ( Nothing, Just _ ) ->
            up

        ( Nothing, Nothing ) ->
            Nothing


trimLeftWhitespace : Int -> String -> String
trimLeftWhitespace count string =
    case count of
        0 ->
            string

        _ ->
            if String.startsWith " " string then
                trimLeftWhitespace
                    (abs count - 1)
                    (String.dropLeft 1 string)
            else
                string
