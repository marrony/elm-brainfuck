all: $(addprefix output/, $(patsubst %.elm, %.html, $(wildcard *.elm)))

output/%.html: %.elm
	elm-make --output $@ $<

clean:
	rm -rf output

