SUFFIXES = .beam .erl
PHONY: run

%.beam: %.erl
	erlc $<

run: run.beam auction.beam seller.beam client.beam
	erl -noshell -s run run -s init stop
