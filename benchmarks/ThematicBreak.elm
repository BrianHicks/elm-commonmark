module ThematicBreak exposing (..)

import Benchmark exposing (Benchmark, benchmark1, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import CommonMark exposing (parseString)


thematicBreak : Benchmark
thematicBreak =
    describe "thematic breaks"
        [ benchmark1 "valid" parseString "---"
        , benchmark1 "invalid" parseString "- - - - a"
        ]


main : BenchmarkProgram
main =
    program thematicBreak
