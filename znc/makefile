
test: test.s
	gcc -g -m64 $^ -o $@

test.s: test.zn znc
	./znc $< $@

