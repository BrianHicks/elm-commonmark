module CommonMarkSpec exposing (..)

import CommonMark exposing (..)
import Expect exposing (Expectation)
import String.Extra exposing (replace)
import Test exposing (..)


{-| The spec specifies a lot of multiline tests. This helper allows us to
express them very close to how the spec does. This helps translate and keep
things up to date more easily.
-}
group : String -> (String -> () -> Expectation) -> List String -> Test
group description tester candidates =
    describe description <|
        List.map
            (\raw ->
                test
                    (testName raw)
                    (tester raw)
            )
            candidates


testName : String -> String
testName raw =
    raw
        |> replace " " "·"
        |> replace "\n" "\\n"


example : Document -> String -> Test
example document raw =
    test (testName raw) <|
        \_ ->
            Expect.equal
                (Ok document)
                (parseString raw)


todo : String -> Test
todo =
    testName >> Test.todo


plaintext : String -> Block
plaintext =
    Plain >> Paragraph


thematicBreak : Test
thematicBreak =
    describe "thematic breaks"
        [ describe "valid characters"
            [ example [ ThematicBreak ] "***"
            , example [ ThematicBreak ] "---"
            , example [ ThematicBreak ] "___"
            ]
        , describe "wrong characters"
            [ example [ plaintext "+++" ] "+++"
            , example [ plaintext "===" ] "==="
            ]
        , describe "not enough characters"
            [ example [ plaintext "--" ] "--"
            , example [ plaintext "**" ] "**"
            , example [ plaintext "__" ] "__"
            ]
        , describe "one to three spaces indent are allowed"
            [ example [ ThematicBreak ] " ***"
            , example [ ThematicBreak ] "  ***"
            , example [ ThematicBreak ] "   ***"
            ]
        , describe "four spaces is too many"
            [ todo "    ***"
            , todo "Foo\n    ***"
            ]
        , describe "more than three characters may be used"
            [ example [ ThematicBreak ] "_____________________________________" ]
        , describe "spaces are allowed between the characters"
            [ example [ ThematicBreak ] " - - -"
            , example [ ThematicBreak ] " **  * ** * ** * **"
            , example [ ThematicBreak ] "-     -     -     -"
            ]
        , describe "spaces are allowed at the end"
            [ example [ ThematicBreak ] "- - - -    " ]
        , describe "no other characters may occur in the line"
            [ example [ plaintext "_ _ _ _ a" ] "_ _ _ _ a"
            , example [ plaintext "a------" ] "a------"
            , example [ plaintext "---a---" ] "---a---"
            ]
        , describe "it is required that all of the non-whitespace characters be the same"
            [ todo " *-*" ]
        , describe "thematic breaks do not need blank lines before or after"
            [ todo "- foo\n***\n- bar" ]
        , describe "when both a thematic break and a setext heading are possible, the setext heading takes precedence"
            [ todo "Foo\n---\nbar" ]
        , describe "if you want a thematic break in a list item, use a different bullet"
            [ todo "- Foo\n- * * *" ]
        ]


atxHeading : Test
atxHeading =
    describe "ATX headings"
        [ describe "simple headings"
            [ example [ Heading 1 (Just <| Plain "foo") ] "# foo"
            , example [ Heading 2 (Just <| Plain "foo") ] "## foo"
            , example [ Heading 3 (Just <| Plain "foo") ] "### foo"
            , example [ Heading 4 (Just <| Plain "foo") ] "#### foo"
            , example [ Heading 5 (Just <| Plain "foo") ] "##### foo"
            , example [ Heading 6 (Just <| Plain "foo") ] "###### foo"
            ]
        , describe "more than 6 characters is not a heading"
            [ example [ plaintext "####### foo" ] "####### foo" ]
        , describe "at least one space is required between the # characters and the heading's contents"
            [ example [ plaintext "#5 bolt" ] "#5 bolt"
            , example [ plaintext "#hashtag" ] "#hashtag"
            ]
        , describe "not a heading if the first # is escaped"
            -- marked as todo because of the escaping
            [ todo "\\## foo" ]

        -- TODO: fix this up when I implement inlines
        , describe "contents are parsed as inlines"
            [ example [ Heading 1 (Just <| Plain "foo *bar* \\*baz\\*") ] "# foo *bar* \\*baz\\*" ]
        , describe "leading and trailing blanks are ignored in parsing inline content"
            [ example [ Heading 1 (Just <| Plain "foo") ] "#                  foo                     " ]
        , describe "one to three spaces indentation are allowed"
            [ example [ Heading 3 (Just <| Plain "foo") ] " ### foo"
            , example [ Heading 2 (Just <| Plain "foo") ] "  ## foo"
            , example [ Heading 1 (Just <| Plain "foo") ] "   # foo"
            ]
        , describe "four spaces are too much"
            [ todo "    # foo"
            , todo "foo\n    # bar"
            ]
        , describe "a closing sequence of # characters is optional"
            [ example [ Heading 2 (Just <| Plain "foo") ] "## foo ##"
            , example [ Heading 3 (Just <| Plain "bar") ] "  ###   bar    ###"
            , describe "it need not be the same length as the opening sequence"
                [ example [ Heading 1 (Just <| Plain "foo") ] "# foo ##################################"
                , example [ Heading 5 (Just <| Plain "foo") ] "##### foo ##"
                ]
            , describe "spaces are allowed after the closing sequence"
                [ example [ Heading 3 (Just <| Plain "foo") ] "### foo ###     " ]
            , describe "a sequence of # characters with anything but spaces following it is not a closing sequence, but counts as part of the contents of the heading"
                [ example [ Heading 3 (Just <| Plain "foo ### b") ] "### foo ### b" ]
            , describe "the closing sequence must be preceded by a space"
                [ example [ Heading 1 (Just <| Plain "foo#") ] "# foo#" ]
            , describe "backslash-escaped # characters do not count as part of the closing sequence"
                [ example [ Heading 3 (Just <| Plain "foo ###") ] "### foo \\###"
                , example [ Heading 2 (Just <| Plain "foo ###") ] "## foo #\\##"
                , example [ Heading 1 (Just <| Plain "foo #") ] "# foo \\#"
                ]
            ]
        , describe "don't need to be separated from surrounding context by blank lines"
            [ example
                [ ThematicBreak
                , Heading 2 (Just <| Plain "foo")
                , ThematicBreak
                ]
                "****\n## foo\n****"
            , example
                [ plaintext "Foo bar"
                , Heading 1 (Just <| Plain "baz")
                , plaintext "Bar foo"
                ]
                "Foo bar\n# baz\nBar foo"
            ]
        , describe "ATX headings can be empty"
            [ example [ Heading 2 Nothing ] "## "
            , example [ Heading 1 Nothing ] "#"
            , example [ Heading 3 Nothing ] "### ###"
            ]
        ]


setextHeading : Test
setextHeading =
    describe "setext headings"
        [ describe "simple headings"
            [ example [ Heading 1 (Just <| Plain "Foo *bar*") ] "Foo *bar*\n========="
            , example [ Heading 2 (Just <| Plain "Foo *bar*") ] "Foo *bar*\n---------"
            ]
        , describe "the content of the header may span more than one line"
            [ example [ Heading 1 (Just <| Plain "Foo *bar\nbaz*") ] "Foo *bar\nbaz*\n===="
            , describe "and, in general, a blank line is not required before the heading"
                [ example
                    [ ThematicBreak
                    , Heading 2 (Just <| Plain "Foo")
                    , Heading 2 (Just <| Plain "Bar")
                    , plaintext "Baz"
                    ]
                    "---\nFoo\n---\nBar\n---\nBaz"
                ]
            ]
        , describe "the underlining can be any length"
            [ example [ Heading 2 (Just <| Plain "Foo") ] "Foo\n-------------------------"
            , example [ Heading 1 (Just <| Plain "Foo") ] "Foo\n="
            ]
        , describe "content can be indented up to three spaces, and need not line up with the underlining"
            [ example [ Heading 2 (Just <| Plain "Foo") ] "   Foo\n---"
            , example [ Heading 2 (Just <| Plain "Foo") ] "  Foo\n-----"
            , example [ Heading 1 (Just <| Plain "Foo") ] "  Foo\n==="
            , describe "four spaces indent is too much"
                [ todo "    Foo\n    ---"
                , todo "    Foo\n---"
                ]
            ]
        , describe "heading underline can be indented up to three spaces, and may have trailing spaces"
            [ example [ Heading 2 (Just <| Plain "Foo") ] "Foo\n   ----      "
            , describe "four spaces is too much"
                [ example [ Paragraph (Plain "Foo\n---") ] "Foo\n    ---" ]
            ]
        , describe "cannot contain internal spaces"
            [ example [ plaintext "Foo\n= =" ] "Foo\n= ="
            , example [ plaintext "Foo", ThematicBreak ] "Foo\n--- -"
            ]
        , describe "trailing spaces in the content line do not cause a line break"
            [ example [ Heading 2 (Just <| Plain "Foo") ] "Foo  \n-----" ]
        , describe "a backslash in the content line do not cause a line break"
            [ todo "Foo\\\n----" ]
        , describe "these are setext headings (block > inline)"
            [ todo "`Foo\n----\n`"
            , todo "<a title=\"a lot\n---\nof dashes\"/>"
            ]
        , describe "underline cannot be a lazy continuation line in a list item or block quote"
            [ todo "> Foo\n---"
            , todo "> foo\nbar\n==="
            , todo "- Foo\n---"
            ]
        , describe "cannot be empty"
            [ example [ plaintext "====" ] "\n====" ]
        , describe "content must be paragraphy"
            [ example [ ThematicBreak, ThematicBreak ] "---\n---"
            , todo "- foo\n-----"
            , todo "    foo\n---"
            , todo "> foo\n-----"
            , example [ Heading 2 (Just <| Plain "\\> foo") ] "\\> foo\n------"
            ]
        , describe "regarding multiline headings and ambiguity"
            [ -- there's some ambiguity about how to interpret multiline
              -- headings between different parsers. We're following the
              -- CommonMark standard, which has a long note about this at the
              -- end of the setext portion of the spec.
              example
                [ plaintext "Foo"
                , Heading 2 (Just <| Plain "bar")
                , plaintext "baz"
                ]
                "Foo\n\nbar\n---\nbaz"
            , example
                [ plaintext "Foo\nbar"
                , ThematicBreak
                , plaintext "baz"
                ]
                "Foo\nbar\n\n---\n\nbaz"
            , example
                [ plaintext "Foo\nbar"
                , ThematicBreak
                , plaintext "baz"
                ]
                "Foo\nbar\n* * *\nbaz"
            , example
                [ plaintext "Foo\nbar\n\\---\nbaz" ]
                "Foo\nbar\n\\---\nbaz"
            ]
        ]
