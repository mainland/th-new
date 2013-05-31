GHC=ghc

GHCFLAGS=-fforce-recomp \
	-dcore-lint \
	-dcmm-lint \
	-rtsopts \
	-Wall \
	-Werror \
	-odir obj -hidir obj \
	-package ghc

.PHONY : all
all : test

.PHONY : clean
clean :
	rm -rf obj test

%.o : %.hs
	$(GHC) $(GHCFLAGS) -c $^

test : Main.hs
	$(GHC) $(GHCFLAGS) -o $@ --make $^
