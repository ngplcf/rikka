GHCFLAGS= -ibin -odir bin -hidir bin -O2
VPATH=src:bin

path=bin
objs=rikka.o data.o
packages=network stm containers

rikka: $(objs)
	ghc $(addprefix bin/,$(objs)) $(addprefix -package ,$(packages)) -o bin/rikka

rikka.o: data.o

%.o: src/%.hs | $(path)
	ghc $(GHCFLAGS) -c $< -o bin/$@

$(path):
	mkdir -p $@

clean:
	rm -rv bin/*
