module CommonMark exposing (Block(..), Document, Inline(..), parseString)

{-| Parse [CommonMark](http://commonmark.org/)-formatted Markdown files.

@docs parseString


## Block-level Elements

@docs Document, Block, Inline

-}

import CommonMark.Block exposing (parseBlockStructure)
import Parser exposing (Error)


{-| turn a CommonMark-formatted into a document

TODO: examples

-}
parseString : String -> Result Error Document
parseString raw =
    raw
        |> String.lines
        |> parseBlockStructure
        |> Result.map parseInlines


{-| a document is just a list of Block-level elements
-}
type alias Document =
    List Block



{- #### implementation note! ####

   There are internal Block and Inline elements in those respectively named
   files. Prefer making modifications there instead of this public API.
-}


{-| Markdown is basically a list of block-level elements which all contain one
or more block- or inline-level elements.
-}
type Block
    = ThematicBreak
      -- TODO: should this carry over the atx/setext designation from the parsing stage?
    | Heading Int (Maybe Inline)
    | Paragraph Inline
    | CodeBlock String


{-| Inline content, for example words in a paragraph or list item
-}
type Inline
    = Plain String


{-| TODO: super incomplete! But this is a stub for the inline parsing when we
get there.
-}
parseInlines : List CommonMark.Block.Block -> List Block
parseInlines =
    List.foldr
        (\block acc ->
            case block of
                CommonMark.Block.ThematicBreak ->
                    ThematicBreak :: acc

                CommonMark.Block.Heading _ level contents ->
                    Heading level (Maybe.map Plain contents) :: acc

                CommonMark.Block.Paragraph contents ->
                    Paragraph (Plain contents) :: acc

                CommonMark.Block.IndentedCodeBlock code ->
                    CodeBlock code :: acc

                CommonMark.Block.BlankLine _ ->
                    acc
        )
        []
