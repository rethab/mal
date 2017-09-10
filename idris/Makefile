# Copied from haskell

#####################


SRCS = step0_repl.idr step1_read_print.idr step2_eval.idr
BINS = $(SRCS:%.idr=%)

#####################

$(BINS): %: %.idr
	idris -p lightyear -o $@ $<

clean:
	rm -f $(BINS) *.ibc *.idr~ *.o
