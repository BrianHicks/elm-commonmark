module Block.FencedCodeBlockSpec exposing (..)

import CommonMark exposing (..)
import CommonMarkSpec exposing (example, plaintext, todo)
import Test exposing (Test, concat, describe)


fencedCodeBlock : Test
fencedCodeBlock =
    concat
        [ describe "simple fenced code blocks"
            [ example [ CodeBlock Nothing "<\n >\n" ] "```\n<\n >\n```"
            , example [ CodeBlock Nothing "<\n >\n" ] "~~~\n<\n >\n~~~"
            ]
        , describe "fewer than three backticks is not enough"
            [ todo "``\nfoo\n``" ]
        , describe "the closing code fence must use the same character as the opening code fence"
            [ example [ CodeBlock Nothing "aaa\n~~~\n" ] "```\naaa\n~~~\n```"
            , example [ CodeBlock Nothing "~~~\n" ] "```\n~~~\n```"
            , example [ CodeBlock Nothing "aaa\n```\n" ] "~~~\naaa\n```\n~~~"
            ]
        , describe "the closing fence must be at least as long as the opening fence"
            [ example [ CodeBlock Nothing "aaa\n```\n" ] "````\naaa\n```\n``````"
            , example [ CodeBlock Nothing "aaa\n~~~\n" ] "~~~~\naaa\n~~~\n~~~~"
            ]
        , describe "unclosed code blocks are closed by the end of the document"
            [ example [ CodeBlock Nothing "" ] "```"
            , example [ CodeBlock Nothing "\n```\naaa\n" ] "`````\n\n```\naaa"
            , todo "> ```\n> aaa\n\nbbb"
            ]
        , describe "a code block can have all empty lines as its content"
            [ example [ CodeBlock Nothing "\n  \n" ] "```\n\n  \n```" ]
        , describe "a code block cNothing an be empty"
            [ example [ CodeBlock Nothing "" ] "```\n```" ]
        , describe "fences can be Nothing indented. If they are, content has that much indentation removed"
            [ example [ CodeBlock Nothing "aaa\naaa\n" ] " ```\n aaa\naaa\n```"
            , example [ CodeBlock Nothing "aaa\naaa\naaa\n" ] "  ```\naaa\n  aaa\naaa\n  ```"
            , example [ CodeBlock Nothing "aaa\n aaa\naaa\n" ] "   ```\n   aaa\n    aaa\n  aaa\n   ```"
            ]
        , describe "closing fences can be indented 1-3 spaces"
            [ example [ CodeBlock Nothing "aaa\n" ] "```\naaa\n  ```"
            , example [ CodeBlock Nothing "aaa\n" ] "   ```\naaa\n  ```"
            , describe "but not 4!"
                [ example [ CodeBlock Nothing "aaa\n    ```\n" ] "```\naaa\n    ```" ]
            ]
        , describe "fences cannot contain internal spaces"
            [ todo "``` ```\naaa"
            , example [ CodeBlock Nothing "aaa\n~~~ ~~\n" ] "~~~~~~\naaa\n~~~ ~~"
            ]
        , describe "fenced code blocks can interrupt paragraphs, and can be followed directly by paragraphs, without a blank line between"
            [ example
                [ plaintext "foo"
                , CodeBlock Nothing "bar\n"
                , plaintext "baz"
                ]
                "foo\n```\nbar\n```\nbaz"
            ]
        , describe "other blocks can also occur before and after fenced code blocks without an intervening blank line"
            [ example
                [ Heading 2 (Just <| Plain "foo")
                , CodeBlock Nothing "bar\n"
                , Heading 1 (Just <| Plain "baz")
                ]
                "foo\n---\n~~~\nbar\n~~~\n# baz"
            ]
        , describe "an info string can be provided after the opening code fence"
            [ example
                [ CodeBlock (Just "ruby") "def foo(x)\n  return 3\nend\n" ]
                "```ruby\ndef foo(x)\n  return 3\nend\n```"
            , example
                [ CodeBlock (Just "ruby") "def foo(x)\n  return 3\nend\n" ]
                "~~~~    ruby startline=3 $%@#$\ndef foo(x)\n  return 3\nend\n~~~~~~~"
            ]
        , describe "info strings cannot contain backticks"
            [ todo "``` aa ```\nfoo" ]
        , describe "closing code fences cannot contain info strings"
            [ example [ CodeBlock Nothing "``` aaa\n" ] "```\n``` aaa\n```" ]
        ]
