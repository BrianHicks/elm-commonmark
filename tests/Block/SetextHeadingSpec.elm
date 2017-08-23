module Block.SetextHeadingSpec exposing (..)

import CommonMark exposing (..)
import CommonMarkSpec exposing (example, plaintext, todo)
import Test exposing (Test, concat, describe)


setextHeading : Test
setextHeading =
    concat
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
                [ example [ CodeBlock "Foo\n---" ] "    Foo\n    ---"
                , example [ CodeBlock "Foo", ThematicBreak ] "    Foo\n---"
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
            [ example [ Heading 2 (Just <| Plain "Foo\\") ] "Foo\\\n----" ]
        , describe "these are setext headings (block > inline)"
            [ example
                [ Heading 2 (Just <| Plain "`Foo")
                , plaintext "`"
                ]
                "`Foo\n----\n`"

            -- TODO: fix this up when I implement inlines
            , example
                [ Heading 2 (Just <| Plain "<a title=\"a lot")
                , plaintext "of dashes\"/>"
                ]
                "<a title=\"a lot\n---\nof dashes\"/>"
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
            , example [ CodeBlock "foo", ThematicBreak ] "    foo\n---"
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
