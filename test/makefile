
ZNC=../znc/znc
# I'm sure there's a better way to do this
ZNC_LUA_PATH=../znc/?.lua

test: test.s
	gcc -g -m64 $^ -o $@

test.s: test.zn
	LUA_PATH=$(ZNC_LUA_PATH) $(ZNC) $< $@

