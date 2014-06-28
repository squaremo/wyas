SRC:=Values.hs Parse.hs Main.hs

.PHONY: all
all: interp

interp: $(SRC)
	ghc --make -o $@ $^
