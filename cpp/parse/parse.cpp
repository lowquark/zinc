
#include <parse/parse.hpp>
#include <parse/lexer.hpp>
#include <pp.hpp>

namespace parse {
  using std::vector;
  using std::unique_ptr;
  using std::make_unique;

  const char * str(token_type type) {
    switch(type) {
      case TOKEN_NULL: return "(null)";
      case TOKEN_EOF: return "EOF";
      case TOKEN_CONST: return "const";
      case TOKEN_FUNCTION: return "function";
      case TOKEN_MODULE: return "module";
      case TOKEN_STRUCT: return "struct";
      case TOKEN_NAME: return "name";
      case TOKEN_INTEGER: return "integer";
      case TOKEN_LCURLY: return "'{'";
      case TOKEN_RCURLY: return "'}'";
      case TOKEN_LPAREN: return "'('";
      case TOKEN_RPAREN: return "')'";
      case TOKEN_LSQUARE: return "'['";
      case TOKEN_RSQUARE: return "']'";
      case TOKEN_LANGLE: return "'<'";
      case TOKEN_RANGLE: return "'>'";
      case TOKEN_LARROW: return "'<-'";
      case TOKEN_RARROW: return "'->'";
      case TOKEN_COMMA: return "','";
      case TOKEN_COLON: return "':'";
      case TOKEN_SEMICOLON: return "';'";
      case TOKEN_EQUALS: return "'='";
      case TOKEN_PLUS: return "'+'";
      case TOKEN_DASH: return "'-'";
      case TOKEN_ASTERISK: return "'*'";
      case TOKEN_SLASH: return "'/'";
      case TOKEN_TILDE: return "'~'";
      case TOKEN_AMPERSAND: return "'&'";
      case TOKEN_LNOT: return "'!'";
      case TOKEN_LOR: return "'||'";
      case TOKEN_LAND: return "'&&'";
      case TOKEN_CEQ: return "'=='";
      case TOKEN_CNEQ: return "'!='";
      case TOKEN_CLEQ: return "'<=";
      case TOKEN_CGEQ: return "'>='";
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

  class context {
    public:
      parse::lexer lexer;

      context(std::istream & is) : lexer(is) { }
  };

  [[noreturn]] void abort_expected(context & ctx, const std::string & exp_str) {
    int line = ctx.lexer.line();
    int col = ctx.lexer.col();
    token_type type = ctx.lexer.next().type;
    std::cerr << "Error " << line << ":" << col << ": Expected " << exp_str <<
                 "; got " << str(type) << std::endl;
    throw std::runtime_error("Compiler error");
  }

  // Parses a token of the given type.
  // If the next lexer token is of the given type, consumes it and returns true.
  // Otherwise, returns false.
  bool parse_token(context & ctx, token_type type) {
    if(ctx.lexer.next().type == type) {
      ctx.lexer.read();
      return true;
    }
    return false;
  }

  // If the next lexer token is of the given type, consumes it.
  // Otherwise, aborts compilation with an error.
  void expect_token(context & ctx, token_type type) {
    if(ctx.lexer.next().type == type) {
      ctx.lexer.read();
    } else {
      abort_expected(ctx, std::string(str(type)));
    }
  }

  unique_ptr<std::string> parse_name(context & ctx) {
    if(ctx.lexer.next().type == TOKEN_NAME) {
      std::string value = ctx.lexer.next().value;
      ctx.lexer.read();
      return make_unique<std::string>(value);
    }
    return nullptr;
  }

  // Expects to parse the rule: <name>
  // If the next lexer token is a string, consumes it and returns its value.
  // Otherwise, aborts compilation with an error.
  std::string expect_name(context & ctx) {
    if(ctx.lexer.next().type == TOKEN_NAME) {
      std::string value = ctx.lexer.next().value;
      ctx.lexer.read();
      return value;
    }
    abort_expected(ctx, std::string(str(TOKEN_NAME)));
  }

  unique_ptr<ast::name_path> parse_name_path(context & ctx) {
    if(ctx.lexer.next().type == TOKEN_NAME) {
      auto path = make_unique<ast::name_path>();
      do {
        // Add this name to the list
        path->push_back(expect_name(ctx));
        // Keep adding names, so long as colons remain
      } while(parse_token(ctx, TOKEN_COLON));
      return path;
    }
    return nullptr;
  }

  // Expects to parse the rule: <name-path>
  ast::name_path expect_name_path(context & ctx) {
    auto path = parse_name_path(ctx);
    if(path) {
      return *path;
    } else {
      abort_expected(ctx, "name path (e.g. foo:bar)");
    }
  }

  // Parses the rule: <type-specifier>
  unique_ptr<ast::type_specifier> parse_type_specifier(context & ctx) {
    // We might backtrack here
    lexer::state st = ctx.lexer.save();

    bool const_qual = parse_token(ctx, TOKEN_CONST);
    auto name_path = parse_name_path(ctx);
    if(!name_path) {
      // Not actually a type, boo
      ctx.lexer.restore(st);
      return nullptr;
    }
    bool ref_qual = parse_token(ctx, TOKEN_AMPERSAND);

    // Type read, return
    auto type = make_unique<ast::type_specifier>();
    type->const_qualified = const_qual;
    type->name_path = std::move(name_path);
    type->reference_qualified = ref_qual;
    return type;
  }

  // Expects to parse the rule: <type-specifier>
  unique_ptr<ast::type_specifier> expect_type_specifier(context & ctx) {
    auto path = parse_type_specifier(ctx);
    if(path) {
      return path;
    } else {
      abort_expected(ctx, "type specifier (e.g. const int &)");
    }
  }

  // -----------------------------------------------------------------------------------------------
  // Module items

  // Tries to parse: <function-declaration> | <function-definition>
  unique_ptr<ast::module_item> parse_function(context & ctx) {
    if(parse_token(ctx, TOKEN_FUNCTION)) {
      // Must have name path / signature
      auto name = expect_name(ctx);

      vector<ast::function_argument> arguments;
      vector<ast::function_return> returns;

      // Arguments expected in parenthesis
      expect_token(ctx, TOKEN_LPAREN);
      while(!parse_token(ctx, TOKEN_RPAREN)) {
        do {
          // Expect & append argument
          ast::function_argument arg;
          arg.type = expect_type_specifier(ctx);
          arg.name = expect_name(ctx);
          arguments.emplace_back(std::move(arg));
        } while(parse_token(ctx, TOKEN_COMMA));
      }

      // If arrow is present, returns expected in parenthesis
      if(parse_token(ctx, TOKEN_RARROW)) {
        expect_token(ctx, TOKEN_LPAREN);
        while(!parse_token(ctx, TOKEN_RPAREN)) {
          do {
            // Expect & append return
            ast::function_return ret;
            ret.type = expect_type_specifier(ctx);
            // Return names are permitted, but ignored.
            parse_name(ctx);
            returns.emplace_back(std::move(ret));
          } while(parse_token(ctx, TOKEN_COMMA));
        }
      }

      // Definition if body present, declaration otherwise
      if(parse_token(ctx, TOKEN_LCURLY)) {
        auto def = make_unique<ast::function_definition>();
        def->name = name;
        while(!parse_token(ctx, TOKEN_RCURLY)) {
          // TODO: Read statements
          ctx.lexer.read();
        }
        return def;
      } else {
        auto decl = make_unique<ast::function_declaration>();
        decl->name = name;
        // Must have semicolon
        expect_token(ctx, TOKEN_SEMICOLON);
        return decl;
      }
    }
    return nullptr;
  }

  // Tries to parse the rule: <member-declaration>
  unique_ptr<ast::module_item> parse_member_declaration(context & ctx) {
    // TODO:
    return nullptr;
  }

  // Expects to parse the rule: <module-item>
  unique_ptr<ast::module_item> expect_module_item(context & ctx) {
    unique_ptr<ast::module_item> item;

    item = parse_function(ctx);
    if(item) { return item; }

    item = parse_member_declaration(ctx);
    if(item) { return item; }

    abort_expected(ctx, "module item (e.g. function or member declaration)");
    return item;
  }

  // -----------------------------------------------------------------------------------------------
  // File items

  // Tries to parse: <struct-declaration> | <struct-definition>
  unique_ptr<ast::file_item> parse_struct(context & ctx) {
    if(parse_token(ctx, TOKEN_STRUCT)) {
      // Must have name path
      auto name = expect_name_path(ctx);

      if(parse_token(ctx, TOKEN_LCURLY)) {
        // Definition
        auto def = make_unique<ast::struct_definition>();
        def->name = name;
        while(!parse_token(ctx, TOKEN_RCURLY)) {
          // TODO: Actually read fields
          ctx.lexer.read();
        }
        return def;
      } else {
        // Declaration
        auto decl = make_unique<ast::struct_declaration>();
        decl->name = name;
        // Must have semicolon
        expect_token(ctx, TOKEN_SEMICOLON);
        return decl;
      }
    }
    return nullptr;
  }

  // Tries to parse the rule: <module-enclosure>
  unique_ptr<ast::file_item> parse_module_enclosure(context & ctx) {
    if(parse_token(ctx, TOKEN_MODULE)) {
      // Create module enclosure
      auto enc = make_unique<ast::module_enclosure>();
      // Read name path
      enc->name = expect_name_path(ctx);
      // Must have lcurly
      expect_token(ctx, TOKEN_LCURLY);
      // Read module items until rcurly
      while(!parse_token(ctx, TOKEN_RCURLY)) {
        enc->items.emplace_back(expect_module_item(ctx));
      }
      return enc;
    }
    return nullptr;
  }

  // Expects to parse the rule: <file-item>
  unique_ptr<ast::file_item> expect_file_item(context & ctx) {
    unique_ptr<ast::file_item> item;

    item = parse_struct(ctx);
    if(item) { return item; }

    item = parse_module_enclosure(ctx);
    if(item) { return item; }

    abort_expected(ctx, "file-scope declaration (e.g. module or struct)");
  }

  // -----------------------------------------------------------------------------------------------
  // File

  // Reads the given stream until EOF, translates it into an abstract syntax tree
  ast::file parse(std::istream & is) {
    ast::file file;

    context ctx(is);
    ctx.lexer.read();

    while(ctx.lexer.next().type != TOKEN_EOF) {
      auto item = expect_file_item(ctx);
      file.items.push_back(std::move(item));
    }

    return file;
  }
}

