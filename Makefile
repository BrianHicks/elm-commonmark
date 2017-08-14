.PHONY: all
all: test documentation.json

elm-stuff: elm-package.json
	elm package install --yes

documentation.json: elm-package.json elm-stuff $(shell find src -type f -name '*.elm')
	elm make --yes --warn --docs=$@

# benchmarks

benchmarks/elm-package.json: elm-package.json
	jq '.["source-directories"] = ["../src", "."] | .dependencies["BrianHicks/elm-benchmark"] = "1.0.2 <= v < 2.0.0"' $< > $@

benchmarks/elm-stuff: benchmarks/elm-package.json
	cd benchmarks; elm package install --yes

benchmarks/%.html: benchmarks/%.elm $(shell find src -type f -name '*.elm') benchmarks/elm-stuff
	cd benchmarks; elm make --output $(shell basename $@) $(shell basename $<)

# test

tests/elm-stuff: tests/elm-package.json
	cd tests; elm package install --yes

.PHONY: test
test: tests/elm-stuff
	elm test

# meta

.PHONY: clean
clean:
	find . -type d -name 'elm-stuff' | xargs rm -rf
	find . -type f -name '*.html' -delete
