#ifndef PARSE_LEXER_HPP
#define PARSE_LEXER_HPP

#include <istream>
#include <string>

namespace parse {
  enum token_type {
    TOKEN_NULL,
    TOKEN_EOF,
    TOKEN_CONST,
    TOKEN_ELSE,
    TOKEN_FUNCTION,
    TOKEN_IF,
    TOKEN_MODULE,
    TOKEN_RETURN,
    TOKEN_STRUCT,
    TOKEN_NAME,
    TOKEN_INTEGER,
    TOKEN_LCURLY,
    TOKEN_RCURLY,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LSQUARE,
    TOKEN_RSQUARE,
    TOKEN_LANGLE,
    TOKEN_RANGLE,
    TOKEN_LARROW,
    TOKEN_RARROW,
    TOKEN_COMMA,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_EQUALS,
    TOKEN_PLUS,
    TOKEN_DASH,
    TOKEN_ASTERISK,
    TOKEN_FSLASH,
    TOKEN_TILDE,
    TOKEN_AMPERSAND,
    TOKEN_LNOT,
    TOKEN_LOR,
    TOKEN_LAND,
    TOKEN_CEQ,
    TOKEN_CNEQ,
    TOKEN_CLEQ,
    TOKEN_CGEQ,
  };

  struct token {
    token_type type;
    std::string value;

    // TODO: Tokens should have line & col instead of lexer

    token(token_type type) : type(type) { }
    token(token_type type, const std::string & val) : type(type), value(val) { }
  };

  class lexer {
    public:
      class state {
        std::streampos pos;
        int line;
        int col;
        int next_char;
        token next_token;

        state(const std::streampos & pos,
              int line,
              int col,
              int next_char,
              const token & next_token)
          : pos(pos),
            line(line),
            col(col),
            next_char(next_char),
            next_token(next_token) { }

        friend class lexer;
      };

      lexer(std::istream & is)
        : is(is), _line(1), _col(0), next_char(-2), next_token(TOKEN_NULL) { }

      // Updates the next token (eats a token from the token stream)
      void read();

      // Save state to backtrack to later
      state save();

      // Backtrack / restore saved state
      void restore(const state & s);

      // The real goods you're looking for
      const token & next() const { return next_token; }
      int line() const { return _line; }
      int col() const { return _col; }

    private:
      std::istream & is;
      int _line;
      int _col;
      int next_char;
      token next_token;

      void eat_char();
  };

  void test_lexer();
}

#endif
