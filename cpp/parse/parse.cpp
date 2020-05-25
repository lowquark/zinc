
#include <parse/parse.hpp>
#include <parse/lexer.hpp>

namespace parse {
  const char * str(token_type type) {
    switch(type) {
      case TOKEN_NULL: return "NULL";
      case TOKEN_EOF: return "EOF";
      case TOKEN_FUNCTION: return "FUNCTION";
      case TOKEN_MODULE: return "MODULE";
      case TOKEN_STRUCT: return "STRUCT";
      case TOKEN_NAME: return "NAME";
      case TOKEN_INTEGER: return "INTEGER";
      case TOKEN_LCURLY: return "LCURLY";
      case TOKEN_RCURLY: return "RCURLY";
      case TOKEN_LPAREN: return "LPAREN";
      case TOKEN_RPAREN: return "RPAREN";
      case TOKEN_LSQUARE: return "LSQUARE";
      case TOKEN_RSQUARE: return "RSQUARE";
      case TOKEN_LANGLE: return "LANGLE";
      case TOKEN_RANGLE: return "RANGLE";
      case TOKEN_COMMA: return "COMMA";
      case TOKEN_COLON: return "COLON";
      case TOKEN_SEMICOLON: return "SEMICOLON";
      case TOKEN_EQUALS: return "EQUALS";
      case TOKEN_PLUS: return "PLUS";
      case TOKEN_DASH: return "DASH";
      case TOKEN_ASTERISK: return "ASTERISK";
      case TOKEN_SLASH: return "SLASH";
      case TOKEN_TILDE: return "TILDE";
      case TOKEN_LNOT: return "LNOT";
      case TOKEN_LOR: return "LOR";
      case TOKEN_LAND: return "LAND";
      case TOKEN_CEQ: return "CEQ";
      case TOKEN_CNEQ: return "CNEQ";
      case TOKEN_CLEQ: return "CLEQ";
      case TOKEN_CGEQ: return "CGEQ";
    }
    throw std::runtime_error("Invalid token type");
  }

  void dump(const token & tok) {
    const char * name = str(tok.type);
    std::cerr << name;
    if(tok.type == TOKEN_INTEGER || 
       tok.type == TOKEN_NAME) {
      std::cerr << " " << tok.value;
    }
    std::cerr << std::endl;
  }

  ast::file parse(std::istream & is) {
    ast::file file;

    lexer l(is);
    l.read();

    while(l.next().type != TOKEN_EOF) {
      dump(l.next());
      l.read();
    }

    return file;
  }
}

