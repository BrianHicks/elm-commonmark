elm-stuff: elm-package.json
	elm package install --yes

benchmarks/elm-package.json: elm-package.json
	jq '.["source-directories"] = ["../src", "."] | .dependencies["BrianHicks/elm-benchmark"] = "1.0.2 <= v < 2.0.0"' $< > $@

benchmarks/elm-stuff: benchmarks/elm-package.json
	cd benchmarks; elm package install --yes

benchmarks/%.html: benchmarks/%.elm benchmarks/elm-stuff
	cd benchmarks; elm make --output $(shell basename $@) $(shell basename $<)

.PHONY: clean
clean:
	find . -type d -name 'elm-stuff' | xargs rm -rf
	find . -type f -name '*.html' -delete

# benchmarks
