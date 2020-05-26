
#include <parse/lexer.hpp>

#include <cassert>
#include <iostream>
#include <sstream>
#include <vector>

namespace parse {
  static bool is_alpha(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
  }

  static bool is_alphanum(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9');
  }

  static bool is_numeric(int c) {
    return (c >= '0' && c <= '9');
  }

  void lexer::read() {
    // Skip initial next_char value
    if(next_char == -2) {
      eat_char();
    }

    // Ignore whitespace
    while(next_char == ' ' || next_char == '\t' || next_char == '\r' || next_char == '\n') {
      if(next_char == '\n') {
        _line += 1;
        _col = 0;
      }
      eat_char();
    }

    if(next_char == -1) {
      // End of file reached
      next_token = token(TOKEN_EOF);
      return;
    }

    if(is_alpha(next_char) || next_char == '_') {
      // This is a name or reserved keyword
      std::string str;

      do {
        str.push_back(next_char);
        eat_char();
      } while(is_alphanum(next_char) || next_char == '_');

      // Check for keywords
      if(str == "module") {
        next_token = token(TOKEN_MODULE);
        return;
      } else if(str == "struct") {
        next_token = token(TOKEN_STRUCT);
        return;
      } else if(str == "const") {
        next_token = token(TOKEN_CONST);
        return;
      } else if(str == "function") {
        next_token = token(TOKEN_FUNCTION);
        return;
      } else {
        // Just a regular old name
        next_token = token(TOKEN_NAME, str);
        return;
      }
    }

    if(is_numeric(next_char)) {
      // This is a number
      std::string str;

      do {
        str.push_back(next_char);
        eat_char();
      } while(is_numeric(next_char));

      next_token = token(TOKEN_INTEGER, str);
      return;
    }

    if(next_char == '{') {
      eat_char();
      next_token = token(TOKEN_LCURLY);
      return;
    }
    if(next_char == '}') {
      eat_char();
      next_token = token(TOKEN_RCURLY);
      return;
    }
    if(next_char == '(') {
      eat_char();
      next_token = token(TOKEN_LPAREN);
      return;
    }
    if(next_char == ')') {
      eat_char();
      next_token = token(TOKEN_RPAREN);
      return;
    }
    if(next_char == '[') {
      eat_char();
      next_token = token(TOKEN_LSQUARE);
      return;
    }
    if(next_char == ']') {
      eat_char();
      next_token = token(TOKEN_RSQUARE);
      return;
    }

    if(next_char == ',') {
      eat_char();
      next_token = token(TOKEN_COMMA);
      return;
    }
    if(next_char == ':') {
      eat_char();
      next_token = token(TOKEN_COLON);
      return;
    }
    if(next_char == ';') {
      eat_char();
      next_token = token(TOKEN_SEMICOLON);
      return;
    }
    if(next_char == '+') {
      eat_char();
      next_token = token(TOKEN_PLUS);
      return;
    }
    if(next_char == '-') {
      eat_char();
      if(next_char == '>') {
        eat_char();
        next_token = token(TOKEN_RARROW);
        return;
      } else {
        next_token = token(TOKEN_DASH);
        return;
      }
    }
    if(next_char == '*') {
      eat_char();
      next_token = token(TOKEN_ASTERISK);
      return;
    }
    if(next_char == '/') {
      eat_char();
      next_token = token(TOKEN_SLASH);
      return;
    }
    if(next_char == '~') {
      eat_char();
      next_token = token(TOKEN_TILDE);
      return;
    }
    if(next_char == '!') {
      eat_char();
      if(next_char == '=') {
        eat_char();
        next_token = token(TOKEN_CNEQ);
        return;
      } else {
        next_token = token(TOKEN_LNOT);
        return;
      }
    }
    if(next_char == '|') {
      eat_char();
      if(next_char == '|') {
        eat_char();
        next_token = token(TOKEN_LOR);
        return;
      } else {
        // TODO: Binary or
        next_token = token(TOKEN_NULL);
        return;
      }
    }
    if(next_char == '&') {
      eat_char();
      if(next_char == '&') {
        eat_char();
        next_token = token(TOKEN_LAND);
        return;
      } else {
        next_token = token(TOKEN_AMPERSAND);
        return;
      }
    }
    if(next_char == '=') {
      eat_char();
      if(next_char == '=') {
        eat_char();
        next_token = token(TOKEN_CEQ);
        return;
      } else {
        next_token = token(TOKEN_EQUALS);
        return;
      }
    }
    if(next_char == '<') {
      eat_char();
      if(next_char == '=') {
        eat_char();
        next_token = token(TOKEN_CLEQ);
        return;
      } else if(next_char == '-') {
        eat_char();
        next_token = token(TOKEN_LARROW);
        return;
      } else {
        next_token = token(TOKEN_LANGLE);
        return;
      }
    }
    if(next_char == '>') {
      eat_char();
      if(next_char == '=') {
        eat_char();
        next_token = token(TOKEN_CGEQ);
        return;
      } else {
        next_token = token(TOKEN_RANGLE);
        return;
      }
    }

    // No match! Invalid token.
    eat_char();
    next_token = token(TOKEN_NULL);
    return;
  }

  // Save state to backtrack to later
  lexer::state lexer::save() {
    return state(is.tellg(), _line, _col, next_char, next_token);
  }

  // Backtrack / restore saved state
  void lexer::restore(const state & s) {
    is.seekg(s.pos);
    _line = s.line;
    _col = s.col;
    next_token = s.next_token;
    next_char = s.next_char;
  }

  void lexer::eat_char() {
    // Read & set
    next_char = is.get();
    // Only increment column for printing characters
    // TODO: Unicode?
    if(next_char >= 32) {
      _col += 1;
    }
  }

  static void verify_tokens(std::stringstream && ss, const std::vector<token> & ref) {
    lexer lex(ss);

    // Initial token should always be null
    assert(lex.next().type == TOKEN_NULL);

    // Ensure all reads match the reference
    for(auto & ref_token : ref) {
      lex.read();
      assert(lex.next().type == ref_token.type);
      assert(lex.next().value == ref_token.value);
    }

    // Ensure last token is EOF
    lex.read();
    assert(lex.next().type == TOKEN_EOF);

    // Ensure last token is EOF, again
    lex.read();
    assert(lex.next().type == TOKEN_EOF);
  }

  void test_lexer() {
    std::vector<token> all_tokens;
    all_tokens.push_back(token(TOKEN_CONST));
    all_tokens.push_back(token(TOKEN_FUNCTION));
    all_tokens.push_back(token(TOKEN_MODULE));
    all_tokens.push_back(token(TOKEN_STRUCT));
    all_tokens.push_back(token(TOKEN_NAME, "bla"));
    all_tokens.push_back(token(TOKEN_INTEGER, "02345"));
    all_tokens.push_back(token(TOKEN_LCURLY));
    all_tokens.push_back(token(TOKEN_RCURLY));
    all_tokens.push_back(token(TOKEN_LPAREN));
    all_tokens.push_back(token(TOKEN_RPAREN));
    all_tokens.push_back(token(TOKEN_LSQUARE));
    all_tokens.push_back(token(TOKEN_RSQUARE));
    all_tokens.push_back(token(TOKEN_LANGLE));
    all_tokens.push_back(token(TOKEN_RANGLE));
    all_tokens.push_back(token(TOKEN_LARROW));
    all_tokens.push_back(token(TOKEN_RARROW));
    all_tokens.push_back(token(TOKEN_COMMA));
    all_tokens.push_back(token(TOKEN_COLON));
    all_tokens.push_back(token(TOKEN_SEMICOLON));
    all_tokens.push_back(token(TOKEN_EQUALS));
    all_tokens.push_back(token(TOKEN_PLUS));
    all_tokens.push_back(token(TOKEN_DASH));
    all_tokens.push_back(token(TOKEN_ASTERISK));
    all_tokens.push_back(token(TOKEN_SLASH));
    all_tokens.push_back(token(TOKEN_TILDE));
    all_tokens.push_back(token(TOKEN_AMPERSAND));
    all_tokens.push_back(token(TOKEN_LNOT));
    all_tokens.push_back(token(TOKEN_LOR));
    all_tokens.push_back(token(TOKEN_LAND));
    all_tokens.push_back(token(TOKEN_CEQ));
    all_tokens.push_back(token(TOKEN_CNEQ));
    all_tokens.push_back(token(TOKEN_CLEQ));
    all_tokens.push_back(token(TOKEN_CGEQ));

    {
      std::stringstream ss;
      ss << "const function module struct bla 02345";
      ss << "{}()[]<><-->";
      ss << ",:;=+-*/~&!||&&==!=<=>=";
      verify_tokens(std::move(ss), all_tokens);
    }

    {
      std::stringstream ss;
      ss << "const\nfunction\r\nmodule\tstruct\tbla\n02345";
      ss << "{ } ( ) [ ] < > <- ->";
      ss << ", :; = +-*/ ~& !|| && == != <= >=";
      verify_tokens(std::move(ss), all_tokens);
    }
  }
}

