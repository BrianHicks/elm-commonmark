module Block.ThematicBreakSpec exposing (..)

import CommonMark exposing (..)
import CommonMarkSpec exposing (example, plaintext, todo)
import Test exposing (Test, concat, describe)


thematicBreak : Test
thematicBreak =
    concat
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
            [ example [ CodeBlock "***" ] "    ***"
            , example [ plaintext "Foo\n***" ] "Foo\n    ***"
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
            [ example
                [ Heading 2 (Just <| Plain "Foo")
                , plaintext "bar"
                ]
                "Foo\n---\nbar"
            ]
        , describe "if you want a thematic break in a list item, use a different bullet"
            [ todo "- Foo\n- * * *" ]
        ]
