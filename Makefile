GHCFLAGS= -ibin -odir bin -hidir bin -O2
VPATH=src:bin

path=bin
objs=rikka.o
packages=network stm

rikka: $(objs)
	ghc $(addprefix bin/,$(objs)) $(addprefix -package ,$(packages)) -o bin/rikka

%.o: src/%.hs | $(path)
	ghc $(GHCFLAGS) -c $< -o bin/$@

$(path):
	mkdir -p $@

clean:
	rm -rv bin/*
