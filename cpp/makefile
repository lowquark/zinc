
%.o: %.cpp
	g++ -g -Wall -Wextra -Wno-unused-parameter -I. -c -o $@ $<

znc: main.o ir.o ast.o pp.o parse/parse.o parse/lexer.o
	g++ -g -o $@ $^

