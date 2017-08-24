module Block.TabSpec exposing (..)

import CommonMark exposing (..)
import CommonMarkSpec exposing (example, plaintext, todo)
import Test exposing (Test, concat, describe)


tabs : Test
tabs =
    concat
        [ describe "a tab can be used instead of four spaces in an indented code block"
            [ example [ CodeBlock Nothing "foo\tbaz\t\tbim" ] "\tfoo\tbaz\t\tbim"
            , example [ CodeBlock Nothing "foo\tbaz\t\tbim" ] "  \tfoo\tbaz\t\tbim"
            , example [ CodeBlock Nothing "a\ta\nὐ\ta" ] "    a\ta\n    ὐ\ta"
            ]
        , describe "a continuation with a tab has exactly the same effect as a continuation with four spaces"
            [ todo "  - foo\n\n\tbar"
            , todo "- foo\n\t\tbar"
            ]
        , describe "tabs are treated as tabstops, in general"
            [ todo ">\t\tfoo"
            , todo "-\t\tfoo"
            , example [ CodeBlock Nothing "foo\nbar" ] "    foo\n\tbar"
            , todo " - foo\n   - bar\n\t - baz"
            , example [ Heading 1 (Just <| Plain "Foo") ] "#\tFoo"
            , example [ ThematicBreak ] "*\t*\t*\t"
            ]
        ]
