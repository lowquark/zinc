
#include <parse/parse.hpp>
#include <parse/lexer.hpp>
#include <pp.hpp>

#include <sstream>

namespace parse {
  using std::vector;
  using std::unique_ptr;
  using std::make_unique;

  const char * str(token_type type) {
    switch(type) {
      case TOKEN_NULL: return "(null)";
      case TOKEN_EOF: return "EOF";
      case TOKEN_CONST: return "const";
      case TOKEN_ELSE: return "else";
      case TOKEN_FUNCTION: return "function";
      case TOKEN_IF: return "if";
      case TOKEN_MODULE: return "module";
      case TOKEN_STRUCT: return "struct";
      case TOKEN_RETURN: return "return";
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
      case TOKEN_FSLASH: return "'/'";
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

  unique_ptr<ast::lvalue> parse_lvalue(context & ctx) {
    auto name = parse_name(ctx);
    if(name) {
      auto lvalue = make_unique<ast::lvalue_referential>();
      lvalue->name = *name;
      return lvalue;
    }
    return nullptr;
  }

  // -----------------------------------------------------------------------------------------------
  // Expressions

  unique_ptr<ast::expression> parse_expression(context & ctx);
  unique_ptr<ast::expression> expect_expression(context & ctx);

  unique_ptr<ast::expression> parse_expression_p0(context & ctx) {
    if(parse_token(ctx, TOKEN_LPAREN)) {
      auto subexpr = expect_expression(ctx);
      expect_token(ctx, TOKEN_RPAREN);
      return subexpr;
    } else if(parse_token(ctx, TOKEN_DASH)) {
      auto subexpr = parse_expression_p0(ctx);
      return make_unique<ast::expression_neg>(std::move(subexpr));
    } else if(parse_token(ctx, TOKEN_TILDE)) {
      auto subexpr = parse_expression_p0(ctx);
      return make_unique<ast::expression_bnot>(std::move(subexpr));
    } else if(parse_token(ctx, TOKEN_LNOT)) {
      auto subexpr = parse_expression_p0(ctx);
      return make_unique<ast::expression_lnot>(std::move(subexpr));
    } else if(ctx.lexer.next().type == TOKEN_INTEGER) {
      int64_t value;
      std::stringstream ss(ctx.lexer.next().value);
      ss >> value;
      ctx.lexer.read();
      return make_unique<ast::expression_integer>(value);
    }
    // <name->path> [ '(' [ <arguments> ] ')' ]
    auto st = ctx.lexer.save();
    auto name_path = parse_name_path(ctx);
    if(name_path) {
      if(parse_token(ctx, TOKEN_LPAREN)) {
        auto expr = make_unique<ast::expression_call>();
        expr->name_path = std::move(name_path);
        while(!parse_token(ctx, TOKEN_RPAREN)) {
          // XXX TODO XXX: Function call expression arguments
          abort_expected(ctx, "STOP RIGHT THERE CRIMINAL SCUM");
        }
        return expr;
      }
    }
    // <lvalue>
    ctx.lexer.restore(st);
    auto lvalue = parse_lvalue(ctx);
    if(lvalue) {
      return make_unique<ast::expression_lvalue>(std::move(lvalue));
    }
    return nullptr;
  }

  unique_ptr<ast::expression> parse_expression_p1(context & ctx) {
    // Parse first argument
    auto subexpr = parse_expression_p0(ctx);
    // Wrap if applicable
    auto op_type = ctx.lexer.next().type;
    while(op_type == TOKEN_ASTERISK || op_type == TOKEN_FSLASH) {
      // Operand match, consume
      ctx.lexer.read();
      // Parse second argument
      auto subexpr2 = parse_expression_p0(ctx);
      // Replace with combined subexpressions
      switch(op_type) {
        case TOKEN_ASTERISK:
          subexpr = make_unique<ast::expression_mul>(std::move(subexpr), std::move(subexpr2));
          break;
        case TOKEN_FSLASH:
          subexpr = make_unique<ast::expression_div>(std::move(subexpr), std::move(subexpr2));
          break;
        default:
          throw;
          break;
      }
      op_type = ctx.lexer.next().type;
    }
    return subexpr;
  }

  unique_ptr<ast::expression> parse_expression_p2(context & ctx) {
    // Parse first argument
    auto subexpr = parse_expression_p1(ctx);
    // Wrap if applicable
    auto op_type = ctx.lexer.next().type;
    while(op_type == TOKEN_PLUS || op_type == TOKEN_DASH) {
      // Operand match, consume
      ctx.lexer.read();
      // Parse second argument
      auto subexpr2 = parse_expression_p1(ctx);
      // Replace with combined subexpressions
      switch(op_type) {
        case TOKEN_PLUS:
          subexpr = make_unique<ast::expression_add>(std::move(subexpr), std::move(subexpr2));
          break;
        case TOKEN_DASH:
          subexpr = make_unique<ast::expression_sub>(std::move(subexpr), std::move(subexpr2));
          break;
        default:
          throw;
          break;
      }
      op_type = ctx.lexer.next().type;
    }
    return subexpr;
  }

  unique_ptr<ast::expression> parse_expression_p3(context & ctx) {
    // Parse first argument
    auto subexpr = parse_expression_p2(ctx);
    // Wrap if applicable
    auto op_type = ctx.lexer.next().type;
    while(op_type == TOKEN_LANGLE ||
          op_type == TOKEN_CLEQ ||
          op_type == TOKEN_RANGLE ||
          op_type == TOKEN_CGEQ) {
      // Operand match, consume
      ctx.lexer.read();
      // Parse second argument
      auto subexpr2 = parse_expression_p2(ctx);
      // Replace with combined subexpressions
      switch(op_type) {
        case TOKEN_LANGLE:
          subexpr = make_unique<ast::expression_clt>(std::move(subexpr), std::move(subexpr2));
          break;
        case TOKEN_CLEQ:
          subexpr = make_unique<ast::expression_cleq>(std::move(subexpr), std::move(subexpr2));
          break;
        case TOKEN_RANGLE:
          subexpr = make_unique<ast::expression_cgt>(std::move(subexpr), std::move(subexpr2));
          break;
        case TOKEN_CGEQ:
          subexpr = make_unique<ast::expression_cgeq>(std::move(subexpr), std::move(subexpr2));
          break;
        default:
          throw;
          break;
      }
      op_type = ctx.lexer.next().type;
    }
    return subexpr;
  }

  unique_ptr<ast::expression> parse_expression_p4(context & ctx) {
    // Parse first argument
    auto subexpr = parse_expression_p3(ctx);
    // Wrap if applicable
    auto op_type = ctx.lexer.next().type;
    while(op_type == TOKEN_CEQ || op_type == TOKEN_CNEQ) {
      // Operand match, consume
      ctx.lexer.read();
      // Parse second argument
      auto subexpr2 = parse_expression_p3(ctx);
      // Replace with combined subexpressions
      switch(op_type) {
        case TOKEN_CEQ:
          subexpr = make_unique<ast::expression_ceq>(std::move(subexpr), std::move(subexpr2));
          break;
        case TOKEN_CNEQ:
          subexpr = make_unique<ast::expression_cneq>(std::move(subexpr), std::move(subexpr2));
          break;
        default:
          throw;
          break;
      }
      op_type = ctx.lexer.next().type;
    }
    return subexpr;
  }

  unique_ptr<ast::expression> parse_expression_p5(context & ctx) {
    // Parse first argument
    auto subexpr = parse_expression_p4(ctx);
    // Wrap if applicable
    auto op_type = ctx.lexer.next().type;
    while(op_type == TOKEN_LAND) {
      // Operand match, consume
      ctx.lexer.read();
      // Parse second argument
      auto subexpr2 = parse_expression_p4(ctx);
      // Replace with combined subexpressions
      switch(op_type) {
        case TOKEN_LAND:
          subexpr = make_unique<ast::expression_land>(std::move(subexpr), std::move(subexpr2));
          break;
        default:
          throw;
          break;
      }
      op_type = ctx.lexer.next().type;
    }
    return subexpr;
  }

  unique_ptr<ast::expression> parse_expression_p6(context & ctx) {
    // Parse first argument
    auto subexpr = parse_expression_p5(ctx);
    // Wrap if applicable
    auto op_type = ctx.lexer.next().type;
    while(op_type == TOKEN_LOR) {
      // Operand match, consume
      ctx.lexer.read();
      // Parse second argument
      auto subexpr2 = parse_expression_p5(ctx);
      // Replace with combined subexpressions
      switch(op_type) {
        case TOKEN_LOR:
          subexpr = make_unique<ast::expression_lor>(std::move(subexpr), std::move(subexpr2));
          break;
        default:
          throw;
          break;
      }
      op_type = ctx.lexer.next().type;
    }
    return subexpr;
  }

  unique_ptr<ast::expression> parse_expression(context & ctx) {
    return parse_expression_p6(ctx);
  }

  unique_ptr<ast::expression> expect_expression(context & ctx) {
    auto expr = parse_expression(ctx);
    if(expr) {
      return expr;
    }
    abort_expected(ctx, "expression (e.g. x*5)");
  }

  // -----------------------------------------------------------------------------------------------
  // Statements

  unique_ptr<ast::statement> expect_statement(context & ctx);
  unique_ptr<ast::statement> parse_if_statement(context & ctx);

  // Tries to parse the rule: <return-statement>
  unique_ptr<ast::statement_return> parse_return_statement(context & ctx) {
    if(parse_token(ctx, TOKEN_RETURN)) {
      auto stmt = make_unique<ast::statement_return>();
      if(!parse_token(ctx, TOKEN_SEMICOLON)) {
        do {
          stmt->expressions.emplace_back(expect_expression(ctx));
        } while(parse_token(ctx, TOKEN_COMMA));
        expect_token(ctx, TOKEN_SEMICOLON);
      }
      return stmt;
    }
    return nullptr;
  }

  // Tries to parse the rule: <block>
  unique_ptr<ast::statement_block> parse_block_statement(context & ctx) {
    if(parse_token(ctx, TOKEN_LCURLY)) {
      auto block = make_unique<ast::statement_block>();
      while(!parse_token(ctx, TOKEN_RCURLY)) {
        block->statements.emplace_back(expect_statement(ctx));
      }
      return block;
    }
    return nullptr;
  }

  // Tries to parse the rule: <block>
  unique_ptr<ast::statement_block> expect_block_statement(context & ctx) {
    auto stmt = parse_block_statement(ctx);
    if(stmt) {
      return stmt;
    }
    abort_expected(ctx, "block (e.g. { })");
  }

  // Tries to parse the rule: <else-body>
  unique_ptr<ast::statement> parse_else_body(context & ctx) {
    if(parse_token(ctx, TOKEN_ELSE)) {
      auto block = parse_if_statement(ctx);
      if(block) {
        return block;
      }
      block = parse_block_statement(ctx);
      if(block) {
        return block;
      }
      abort_expected(ctx, "else block (e.g. `else if(...) { ... }` or `else { ... }`)");
    }
    return nullptr;
  }

  // Tries to parse the rule: <if-statement>
  unique_ptr<ast::statement> parse_if_statement(context & ctx) {
    if(parse_token(ctx, TOKEN_IF)) {
      auto stmt = make_unique<ast::statement_if>();
      expect_token(ctx, TOKEN_LPAREN);
      stmt->condition = expect_expression(ctx);
      expect_token(ctx, TOKEN_RPAREN);
      stmt->if_block = expect_block_statement(ctx);
      stmt->else_block = parse_else_body(ctx);
      return stmt;
    }
    return nullptr;
  }

  // Tries to parse the rule: <function-call>
  unique_ptr<ast::statement> parse_call_statement(context & ctx) {
    // TODO:
    return nullptr;
  }

  // Tries to parse the rule: <assignment>
  unique_ptr<ast::statement> parse_assignment(context & ctx) {
    // TODO:
    return nullptr;
  }

  // Expects to parse the rule: <module-item>
  unique_ptr<ast::statement> expect_statement(context & ctx) {
    unique_ptr<ast::statement> item;

    item = parse_return_statement(ctx);
    if(item) { return item; }

    item = parse_block_statement(ctx);
    if(item) { return item; }

    item = parse_if_statement(ctx);
    if(item) { return item; }

    item = parse_call_statement(ctx);
    if(item) { return item; }

    item = parse_assignment(ctx);
    if(item) { return item; }

    abort_expected(ctx, "statement (e.g. `x = 5;`)");
    return item;
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

      // Arguments are expected in parenthesis
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

      // If arrow is present, returns are expected in parenthesis
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

      auto block = parse_block_statement(ctx);

      // Definition if body present, declaration otherwise
      if(block) {
        auto def = make_unique<ast::function_definition>();
        def->name = name;
        def->block = std::move(block);
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

