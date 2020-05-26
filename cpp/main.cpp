
#include <ast.hpp>
#include <ir.hpp>
#include <parse/parse.hpp>
#include <parse/lexer.hpp>
#include <pp.hpp>

#include <fstream>

int main(int argc, char ** argv) {
  ir::subroutine subr;

  subr.name = "z$my_module$main";
  subr.num_arguments = 2;
  subr.num_returns = 2;

  ir::append_statement(subr, ir::add(ir::reg(0), ir::lvar(0), ir::lvar(1)));
  ir::append_statement(subr, ir::sub(ir::reg(1), ir::reg(0), ir::lvar(2)));
  ir::append_statement(subr, ir::div(ir::reg(2), ir::reg(1), ir::gvar(0)));
  ir::append_statement(subr, ir::label(0));
  ir::append_statement(subr, ir::call(0, 2, 2));
  ir::append_statement(subr, ir::callarg(ir::reg(0)));
  ir::append_statement(subr, ir::callarg(ir::reg(1)));
  ir::append_statement(subr, ir::callres(ir::reg(2)));
  ir::append_statement(subr, ir::callres(ir::reg(3)));
  ir::append_statement(subr, ir::lor(ir::reg(2), ir::reg(1), ir::gvar(0)));

  pp::dump(subr);
  pp::dump(ir::argument_reg(subr, 0));
  pp::dump(ir::argument_reg(subr, 1));
  pp::dump(ir::return_reg(subr, 0));
  pp::dump(ir::return_reg(subr, 1));
  pp::dump(ir::temporary_reg(subr, 0));

  parse::test_lexer();

  std::fstream file("./test.zn");

  ast::file ast = parse::parse(file);

  /*
  ast::name_path name;
  name.emplace_back("aa");
  name.emplace_back("bb");
  name.emplace_back("cc");

  auto enc = std::make_unique<ast::module_enclosure>();
  enc->name = name;
  for(int i = 0 ; i < 3 ; ++i) {
    auto def = std::make_unique<ast::function_definition>();
    def->name = "asdf";
    for(int k = 0 ; k < 2 ; ++k) {
      // Build an expression list
      ast::expression_list exprs;
      for(int j = 0 ; j < 2 ; ++j) {
        auto expr = std::make_unique<ast::expression_integer>();
        expr->value = j;
        exprs.emplace_back(std::move(expr));
      }
      // Place in new return statement
      auto stmt = std::make_unique<ast::statement_return>();
      stmt->expressions = std::move(exprs);
      // Append to function statements
      def->statements.emplace_back(std::move(stmt));
    }
    enc->items.emplace_back(std::move(def));
  }
  ast.items.emplace_back(std::move(enc));
  */

  pp::dump(ast);

  return 0;
}

