
.PHONY: all
all: test.s my_test

test.s: test.c
	gcc -S -m64 -o $@ $^

my_test: my_test.s
	gcc -g -m64 -o $@ $^

