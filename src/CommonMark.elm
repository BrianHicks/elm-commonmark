module CommonMark exposing (Block(..), Document, Inline(..), parseString)

{-| Parse [CommonMark](http://commonmark.org/)-formatted Markdown files.

@docs parseString


## Block-level Elements

@docs Document, Block

-}

import Parser exposing (..)
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
    | Heading Int (Maybe Inline)
    | Paragraph Inline


type Inline
    = Plain String


parseString : String -> Result Error Document
parseString raw =
    raw
        |> String.lines
        |> parseBlockStructure



-- blocks


type BlockNode
    = Open Block
    | Closed Block


finalizeBlockNode : BlockNode -> Block
finalizeBlockNode blockNode =
    case blockNode of
        Open block ->
            block

        Closed block ->
            block


parseBlockStructure : List String -> Result Error Document
parseBlockStructure lines =
    let
        helper : ( Int, String ) -> Result Error (List BlockNode) -> Result Error (List BlockNode)
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
        |> Result.map (List.map finalizeBlockNode)
        |> Result.map List.reverse


parseBlockLine : String -> List BlockNode -> Result Error (List BlockNode)
parseBlockLine line document =
    let
        possibilities : Parser BlockNode
        possibilities =
            oneOf
                [ if List.isEmpty document then
                    fail "setext headings are not possible for first lines"
                  else
                    setextHeading
                , atxHeading
                , thematicBreak
                , succeed <| Open <| Paragraph <| Plain line
                ]

        closeOrExtendHead : List BlockNode -> BlockNode -> List BlockNode
        closeOrExtendHead document block =
            case ( document, block ) of
                -- add a new line onto a paragraph
                ( (Open (Paragraph (Plain content))) :: rest, Open (Paragraph (Plain moreContent)) ) ->
                    Open (Paragraph (Plain <| content ++ "\n" ++ moreContent)) :: rest

                -- combine a single-line paragraph with a setext heading
                ( (Open (Paragraph content)) :: rest, Open (Heading level _) ) ->
                    Open (Heading level (Just content)) :: rest

                -- convert a setext heading to a plain paragraph otherwise
                ( _, Open (Heading level _) ) ->
                    Open (Paragraph (Plain line)) :: document

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


thematicBreak : Parser BlockNode
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


atxHeading : Parser BlockNode
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
                                Just (Plain stuff)
                        )
                )


{-| since we're parsing line-by-line, setext headings just need to find the
underline character, but produce a heading. So we use an empty Heading for that
purpose.
-}
setextHeading : Parser BlockNode
setextHeading =
    let
        level : Parser Int
        level =
            oneOf
                [ succeed 1
                    |. ignore oneOrMore ((==) '=')
                    |. end
                , succeed 2
                    |. ignore oneOrMore ((==) '-')
                    |. end
                ]
    in
    inContext "in a potential setext heading" <|
        map Open <|
            delayedCommitMap Heading
                level
                (succeed Nothing)
