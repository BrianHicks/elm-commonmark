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
            [ example [ plaintext " *-*" ] " *-*" ]
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
            [ example [ Heading 1 (Just <| Plain "Foo *bar\nbaz*") ] "Foo *bar\nbaz*\n====" ]
        ]
