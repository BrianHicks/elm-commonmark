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


testName : String -> String
testName raw =
    raw |> replace " " "·"


testParsed : Document -> String -> Test
testParsed document raw =
    test (testName raw) <|
        \_ ->
            Expect.equal
                (Ok document)
                (parseString raw)


testError : String -> Test
testError badRaw =
    -- TODO: most things that use this should actually be paragraphs. I'd like
    -- to get rid of this eventually!
    test (testName badRaw) <|
        \_ ->
            case parseString badRaw of
                Err _ ->
                    Expect.pass

                Ok [] ->
                    Expect.pass

                Ok stuff ->
                    Expect.fail <| "expected to fail, but got `" ++ toString stuff ++ "`. This probably means you should use `testParsed` instead!"


thematicBreak : Test
thematicBreak =
    let
        isOnlyThematicBreak : String -> () -> Expectation
        isOnlyThematicBreak raw _ =
            -- reversed because the ordering is built for pipelines
            Expect.equal
                (Ok [ ThematicBreak ])
                (parseString raw)

        isNotThematicBreak : String -> () -> Expectation
        isNotThematicBreak raw _ =
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

        containsThematicBreak : String -> () -> Expectation
        containsThematicBreak raw _ =
            -- TODO: revisit this once other block-level elements are
            -- implemented. Right now it's an error since only thematic breaks
            -- are implemented.
            Expect.pass
    in
    describe "thematic breaks"
        [ group "valid characters"
            isOnlyThematicBreak
            [ "***", "---", "___" ]
        , group "wrong characters"
            isNotThematicBreak
            [ "+++", "===" ]
        , group "not enough characters"
            isNotThematicBreak
            [ "--", "**", "__" ]
        , group "one to three spaces indent are allowed"
            isOnlyThematicBreak
            [ " ***", "  ***", "   ***" ]
        , group "four spaces is too many"
            isNotThematicBreak
            [ "    ***", "Foo\n    ***" ]
        , group "more than three characters may be used"
            isOnlyThematicBreak
            [ "_____________________________________" ]
        , group "spaces are allowed between the characters"
            isOnlyThematicBreak
            [ " - - -"
            , " **  * ** * ** * **"
            , "-     -     -     -"
            ]
        , group "spaces are allowed at the end"
            isOnlyThematicBreak
            [ "- - - -    " ]
        , group "no other characters may occur in the line"
            isNotThematicBreak
            [ "_ _ _ _ a"
            , "a------"
            , "---a---"
            ]
        , group "it is required that all of the non-whitespace characters be the same"
            isNotThematicBreak
            [ " *-*" ]
        , group "thematic breaks do not need blank lines before or after"
            containsThematicBreak
            [ "- foo\n***\n- bar" ]
        , group "when both a thematic break and a setext heading are possible, the setext heading takes precedence"
            isNotThematicBreak
            [ "Foo\n---\nbar" ]
        , group "when both a thematic break and a list item are possible, the thematic break takes precedence"
            containsThematicBreak
            [ "* Foo\n* * *\n* Bar" ]
        , group "if you want a thematic break in a list item, use a different bullet"
            containsThematicBreak
            [ "- Foo\n- * * *" ]
        ]


atxHeading : Test
atxHeading =
    let
        isNotAtxHeading : String -> () -> Expectation
        isNotAtxHeading raw _ =
            case parseString raw of
                Ok [ Heading _ _ ] ->
                    Expect.fail "got a heading"

                Ok _ ->
                    Expect.pass

                Err _ ->
                    -- TODO: revisit this when the parser is more fully
                    -- implemented. Should it fail with the error?
                    Expect.pass
    in
    describe "ATX headings"
        [ describe "simple headings"
            [ testParsed [ Heading 1 "foo" ] "# foo"
            , testParsed [ Heading 2 "foo" ] "## foo"
            , testParsed [ Heading 3 "foo" ] "### foo"
            , testParsed [ Heading 4 "foo" ] "#### foo"
            , testParsed [ Heading 5 "foo" ] "##### foo"
            , testParsed [ Heading 6 "foo" ] "###### foo"
            ]
        , describe "more than 6 characters is not a heading"
            [ testError "####### foo" ]
        , describe "at least one space is required between the # characters and the heading's contents"
            [ testError "#5 bolt"
            , testError "#hashtag"
            ]
        , describe "not a heading if the first # is escaped"
            [ testError "\\## foo" ]

        -- TODO: fix this up when I implement inlines
        , describe "contents are parsed as inlines"
            [ testParsed [ Heading 1 "foo *bar* \\*baz\\*" ] "# foo *bar* \\*baz\\*" ]
        , describe "leading and trailing blanks are ignored in parsing inline content"
            [ testParsed [ Heading 1 "foo" ] "#                  foo                     " ]
        , describe "one to three spaces indentation are allowed"
            [ testParsed [ Heading 3 "foo" ] " ### foo"
            , testParsed [ Heading 2 "foo" ] "  ## foo"
            , testParsed [ Heading 1 "foo" ] "   # foo"
            ]
        , describe "four spaces are too much"
            [ testError "    # foo"
            , testError "foo\n    # bar"
            ]
        , describe "a closing sequence of # characters is optional"
            [ testParsed [ Heading 2 "foo" ] "## foo ##"
            , testParsed [ Heading 3 "bar" ] "  ###   bar    ###"
            , describe "it need not be the same length as the opening sequence"
                [ testParsed [ Heading 1 "foo" ] "# foo ##################################"
                , testParsed [ Heading 5 "foo" ] "##### foo ##"
                ]
            , describe "spaces are allowed after the closing sequence"
                [ testParsed [ Heading 3 "foo" ] "### foo ###     " ]
            , describe "a sequence of # characters with anything but spaces following it is not a closing sequence, but counts as part of the contents of the heading"
                [ testParsed [ Heading 3 "foo ### b" ] "### foo ### b" ]
            , describe "the closing sequence must be preceded by a space"
                [ testParsed [ Heading 1 "foo#" ] "# foo#" ]
            , describe "backslash-escaped # characters do not count as part of the closing sequence"
                [ testParsed [ Heading 3 "foo ###" ] "### foo \\###"
                , testParsed [ Heading 2 "foo ###" ] "## foo #\\##"
                , testParsed [ Heading 1 "foo #" ] "# foo \\#"
                ]
            ]
        , describe "don't need to be separated from surrounding context by blank lines"
            [ testParsed
                [ ThematicBreak
                , Heading 2 "foo"
                , ThematicBreak
                ]
                "****\n## foo\n****"
            ]
        ]
