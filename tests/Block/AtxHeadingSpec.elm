module Block.AtxHeadingSpec exposing (..)

import CommonMark exposing (..)
import CommonMarkSpec exposing (example, plaintext, todo)
import Test exposing (Test, concat, describe)


atxHeading : Test
atxHeading =
    concat
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

        -- TODO: fix this up when I implement inlines
        , describe "not a heading if the first # is escaped"
            [ example [ plaintext "\\## foo" ] "\\## foo" ]

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
            [ example [ CodeBlock "# foo" ] "    # foo"
            , example [ plaintext "foo\n# bar" ] "foo\n    # bar"
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
