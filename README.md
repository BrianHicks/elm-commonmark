# CommonMark [![Build Status](https://travis-ci.org/BrianHicks/elm-commonmark.svg?branch=master)](https://travis-ci.org/BrianHicks/elm-commonmark)

An implementation of the [CommonMark spec](http://commonmark.org/) in [Elm](http://elm-lang.org/).

## Project Status

Woefully incomplete. Don't try to use this until it's published on [package](http://package.elm-lang.org/).

### Leaf Blocks

- [x] Thematic Breaks
- [x] ATX Headings
- [x] Setext headings
- [ ] Indented code blocks
- [ ] Fenced code blocks
- [ ] HTML blocks
- [ ] Link reference definitions
- [ ] Paragraphs
- [ ] Blank lines

### Container Blocks

- [ ] Block quotes
- [ ] List items
- [ ] Lists

### Inlines

- [ ] Backslash escapes
- [ ] Entity and numeric character references
- [ ] Code spans
- [ ] Emphasis and strong emphasis
- [ ] Links
- [ ] Images
- [ ] Autolinks
- [ ] Raw HTML
- [ ] Hard line breaks
- [ ] Soft line breaks
- [ ] Textual content

## How do it do?

Following the suggested parsing strategy in the [CommonMark Spec Appendix](http://spec.commonmark.org/0.28/#appendix-a-parsing-strategy).
