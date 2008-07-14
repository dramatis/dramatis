SUFFIXES = .beam .erl
PHONY: run

%.beam: %.erl
	erlc $<

run: play.beam player.beam
	erl -noshell -s play play -s init stop
