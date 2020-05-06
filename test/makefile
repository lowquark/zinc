
ZNC=../znc/znc
# I'm sure there's a better way to do this
ZNC_LUA_PATH=../znc/?.lua

.PHONY: bench clean

bench: bench.s
	gcc -g -m64 $^ -o $@

bench.s: bench.zn
	LUA_PATH=$(ZNC_LUA_PATH) $(ZNC) $< $@

clean:
	rm -f bench
	rm -f bench.s

