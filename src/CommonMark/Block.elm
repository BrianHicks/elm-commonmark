module CommonMark.Block exposing (..)

import Parser exposing (..)
import String.Extra


-- blocks


type Block
    = ThematicBreak
    | Heading Int (Maybe String)
    | Paragraph String


type Node
    = Open Block
    | Closed Block


finalizeNode : Node -> Block
finalizeNode blockNode =
    case blockNode of
        Open block ->
            block

        Closed block ->
            block


parseBlockStructure : List String -> Result Error (List Block)
parseBlockStructure lines =
    let
        helper : ( Int, String ) -> Result Error (List Node) -> Result Error (List Node)
        helper ( rowNumber, line ) progressOrError =
            case Result.andThen (parseBlockLine line) progressOrError of
                Ok newDocument ->
                    Ok newDocument

                Err err ->
                    Err { err | row = rowNumber }
    in
    lines
        |> List.indexedMap (\rowNumber line -> ( rowNumber + 1, line ))
        |> List.foldl helper (Ok [])
        |> Result.map (List.map finalizeNode)
        |> Result.map List.reverse


parseBlockLine : String -> List Node -> Result Error (List Node)
parseBlockLine line document =
    let
        possibilities : Parser Node
        possibilities =
            oneOf
                [ case List.head document of
                    Just (Open (Paragraph _)) ->
                        setextHeading

                    _ ->
                        fail "setext headings can only follow open paragraphs"

                -- and now the things that aren't dependent on the preceding content...
                , atxHeading
                , thematicBreak
                , succeed <| Open <| Paragraph <| String.trim line
                ]

        closeOrExtendHead : List Node -> Node -> List Node
        closeOrExtendHead document block =
            case ( document, block ) of
                -- add a new line onto a paragraph
                ( (Open (Paragraph content)) :: rest, Open (Paragraph moreContent) ) ->
                    Open (Paragraph (content ++ "\n" ++ moreContent)) :: rest

                -- combine a single-line paragraph with a setext heading
                ( (Open (Paragraph content)) :: rest, Open (Heading level _) ) ->
                    Open (Heading level (Just content)) :: rest

                -- convert a setext heading to a plain paragraph otherwise
                ( _, Open (Heading level _) ) ->
                    Open (Paragraph line) :: document

                ( [], _ ) ->
                    [ block ]

                ( anythingElse, _ ) ->
                    block :: anythingElse
    in
    if line == "" then
        case document of
            (Open node) :: rest ->
                Ok <| Closed node :: rest

            _ ->
                Ok <| document
    else
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


thematicBreak : Parser Node
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
        succeed (Closed ThematicBreak)
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
        map Closed <|
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
        map Open <|
            delayedCommitMap Heading
                (succeed identity
                    |. oneToThreeSpaces
                    |= level
                    |. ignore zeroOrMore whitespace
                    |. end
                )
                (succeed Nothing)
