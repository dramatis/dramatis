fail:
	@echo "this example is currently incomplete because of the mistmatch"
	@echo "between the auction example and OTP"
	exit -1

SUFFIXES = .beam .erl
PHONY: run

%.beam: %.erl
	erlc $<

run: run.beam auction.beam seller.beam client.beam
	erl -noshell -s run run -s init stop
