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
                    (raw
                        |> replace " " "·"
                    )
                    (tester raw)
            )
            candidates


thematicBreak : Test
thematicBreak =
    let
        isOnly : String -> () -> Expectation
        isOnly raw unitToSatisfyTestType =
            -- reversed because the ordering is built for pipelines
            Expect.equal
                (Ok [ ThematicBreak ])
                (parseString raw)

        isNot : String -> () -> Expectation
        isNot raw unitToSatisfyTestType =
            case parseString raw |> Result.map List.head of
                Ok (Just ThematicBreak) ->
                    Expect.fail "got a thematic break"

                Ok (Just _) ->
                    Expect.pass

                Ok Nothing ->
                    Expect.pass

                Err _ ->
                    -- TODO: revisit this when the parser is more fully
                    -- implemented. Should it fail with the error?
                    Expect.pass

        contains : String -> () -> Expectation
        contains raw unitToSatisfyTestType =
            -- TODO: revisit this once other block-level elements are
            -- implemented. Right now it's an error since only thematic breaks
            -- are implemented.
            Expect.pass
    in
    describe "thematic breaks"
        [ group "valid characters"
            isOnly
            [ "***", "---", "___" ]
        , group "wrong characters"
            isNot
            [ "+++", "===" ]
        , group "not enough characters"
            isNot
            [ "--", "**", "__" ]
        , group "one to three spaces indent are allowed"
            isOnly
            [ " ***", "  ***", "   ***" ]
        , group "four spaces is too many"
            isNot
            [ "    ***", "Foo\n    ***" ]
        , group "more than three characters may be used"
            isOnly
            [ "_____________________________________" ]
        , group "spaces are allowed between the characters"
            isOnly
            [ " - - -"
            , " **  * ** * ** * **"
            , "-     -     -     -"
            ]
        , group "spaces are allowed at the end"
            isOnly
            [ "- - - -    " ]
        , group "no other characters may occur in the line"
            isNot
            [ "_ _ _ _ a"
            , "a------"
            , "---a---"
            ]
        , group "it is required that all of the non-whitespace characters be the same"
            isNot
            [ " *-*" ]
        , group "thematic breaks do not need blank lines before or after"
            contains
            [ "- foo\n***\n- bar" ]
        , group "when both a thematic break and a setext heading are possible, the setext heading takes precedence"
            isNot
            [ "Foo\n---\nbar" ]
        , group "when both a thematic break and a list item are possible, the thematic break takes precedence"
            contains
            [ "* Foo\n* * *\n* Bar" ]
        , group "if you want a thematic break in a list item, use a different bullet"
            contains
            [ "- Foo\n- * * *" ]
        ]
