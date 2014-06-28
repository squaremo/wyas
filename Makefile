SRC:=Values.hs Parse.hs Eval.hs Main.hs

.PHONY: all
all: interp

interp: $(SRC)
	ghc --make -o $@ $^
