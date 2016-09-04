
all: contest.cmp

clean:
	rm -f min-rt-origin* min-rt-hs* *.ppm *.cmp

%.cmp: %-origin.ppm %-hs.ppm
	diff $^ > $@


#origin
contest-origin.ppm: min-rt-origin contest.sld
	./min-rt-origin < contest.sld > $@

min-rt-origin: min-rt-origin.s globals.s ../libmincaml.S ../stub.c
	gcc -m32 -g -O2 -Wall $^ -lm -o $@

min-rt-origin.s: min-caml min-rt.ml
	./min-caml -inline 100 min-rt
	mv min-rt.s $@


#hs
contest-hs.ppm: min-rt-hs contest.sld
	./min-rt-hs < contest.sld > $@

min-rt-hs: min-rt-hs.s globals.s ../libmincaml.S ../stub.c
	gcc -m32 -g -O2 -Wall $^ -lm -o $@

min-rt-hs.s: min-rt.ml
	stack exec -- min-caml-hs --inline 100 min-rt
	mv min-rt.s $@

