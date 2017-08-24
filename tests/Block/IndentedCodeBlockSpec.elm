module Block.IndentedCodeBlockSpec exposing (..)

import CommonMark exposing (..)
import CommonMarkSpec exposing (example, plaintext, todo)
import Test exposing (Test, concat, describe)


indentedCodeBlocks : Test
indentedCodeBlocks =
    concat
        [ describe "simple examples"
            [ example
                [ CodeBlock Nothing "a simple\n  indented code block" ]
                "    a simple\n      indented code block"
            ]
        , describe "lists take priority over code blocks"
            [ todo "  - foo\n\n    bar"
            , todo "1.  foo\n\n    - bar"
            ]
        , describe "contents are literal, not parsed as Markdown"
            [ example
                -- TODO: the < and > need to be turned into lt and gt entities
                [ CodeBlock Nothing "<a/>\n*hi*\n\n- one" ]
                "    <a/>\n    *hi*\n\n    - one"
            ]
        , describe "blank/empty/low space lines just extend the code block"
            [ example
                [ CodeBlock Nothing "chunk1\n\nchunk2\n\n\n\nchunk3" ]
                "    chunk1\n\n    chunk2\n  \n \n \n    chunk3"
            , describe "any initial spaces beyond four will be included in the content, even in interior blank lines"
                [ example [ CodeBlock Nothing "chunk1\n  \n  chunk2" ] "    chunk1\n      \n      chunk2" ]
            ]
        , describe "an indented code block cannot interrupt a paragraph"
            [ example [ plaintext "Foo\nbar" ] "Foo\n    bar" ]
        , describe "any non-blank line with fewer than four leading spaces ends the code block immediately"
            [ example [ CodeBlock Nothing "foo", plaintext "bar" ] "    foo\nbar" ]
        , describe "indented code can occur immediately before and after other kinds of blocks"
            [ example
                [ Heading 1 (Just <| Plain "Heading")
                , CodeBlock Nothing "foo"
                , Heading 2 (Just <| Plain "Heading")
                , CodeBlock Nothing "foo"
                , ThematicBreak
                ]
                "# Heading\n    foo\nHeading\n------\n    foo\n----"
            ]
        , describe "the first line can be indented more than 4 spaces"
            [ example [ CodeBlock Nothing "    foo\nbar" ] "        foo\n    bar" ]
        , describe "blank lines before or after a code block are not included in it"
            [ example [ CodeBlock Nothing "foo" ] "\n    \n    foo\n    " ]
        , describe "trailing spaces are included in the content"
            [ example [ CodeBlock Nothing "foo  " ] "    foo  " ]
        ]
