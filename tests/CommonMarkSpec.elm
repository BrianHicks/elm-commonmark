module CommonMarkSpec exposing (example, plaintext, todo)

import CommonMark exposing (..)
import Expect exposing (Expectation)
import Regex exposing (regex, replace)
import Test exposing (..)


testName : String -> String
testName raw =
    raw
        |> replace Regex.All (regex " ") (\_ -> "·")
        |> replace Regex.All (regex "\n") (\_ -> "\\n")


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
