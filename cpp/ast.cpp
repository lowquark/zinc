
#include <ast.hpp>

#include <iostream>

namespace ast {
  void visit(const lvalue & stmt, const lvalue_visitor & vis) {
    switch(stmt.variant) {
      case ast::lvalue::DECLARATIONAL:
        vis.visit(static_cast<const ast::lvalue_declarational &>(stmt));
        break;
      case ast::lvalue::REFERENTIAL:
        vis.visit(static_cast<const ast::lvalue_referential &>(stmt));
        break;
    }
  }

  void visit(const expression & expr, const expression_visitor & vis) {
    switch(expr.variant) {
      case ast::expression::INTEGER:
        vis.visit(static_cast<const ast::expression_integer &>(expr));
        break;

      case ast::expression::NEG:
        vis.visit(static_cast<const ast::expression_neg &>(expr));
        break;
      case ast::expression::BNOT:
        vis.visit(static_cast<const ast::expression_bnot &>(expr));
        break;
      case ast::expression::LNOT:
        vis.visit(static_cast<const ast::expression_lnot &>(expr));
        break;

      case ast::expression::ADD:
        vis.visit(static_cast<const ast::expression_add &>(expr));
        break;
      case ast::expression::SUB:
        vis.visit(static_cast<const ast::expression_sub &>(expr));
        break;
      case ast::expression::MUL:
        vis.visit(static_cast<const ast::expression_mul &>(expr));
        break;
      case ast::expression::DIV:
        vis.visit(static_cast<const ast::expression_div &>(expr));
        break;
      case ast::expression::LAND:
        vis.visit(static_cast<const ast::expression_land &>(expr));
        break;
      case ast::expression::LOR:
        vis.visit(static_cast<const ast::expression_lor &>(expr));
        break;

      case ast::expression::CEQ:
        vis.visit(static_cast<const ast::expression_ceq &>(expr));
        break;
      case ast::expression::CLT:
        vis.visit(static_cast<const ast::expression_clt &>(expr));
        break;
      case ast::expression::CLEQ:
        vis.visit(static_cast<const ast::expression_cleq &>(expr));
        break;
      case ast::expression::CGT:
        vis.visit(static_cast<const ast::expression_cgt &>(expr));
        break;
      case ast::expression::CGEQ:
        vis.visit(static_cast<const ast::expression_cgeq &>(expr));
        break;

      case ast::expression::LVALUE:
        vis.visit(static_cast<const ast::expression_lvalue &>(expr));
        break;
      case ast::expression::CALL:
        vis.visit(static_cast<const ast::expression_call &>(expr));
        break;
    }
  }

  void visit(const statement & stmt, const statement_visitor & vis) {
    switch(stmt.variant) {
      case ast::statement::ASSIGNMENT:
        vis.visit(static_cast<const ast::statement_assignment &>(stmt));
        break;
      case ast::statement::BLOCK:
        vis.visit(static_cast<const ast::statement_block &>(stmt));
        break;
      case ast::statement::CALL:
        vis.visit(static_cast<const ast::statement_call &>(stmt));
        break;
      case ast::statement::IF:
        vis.visit(static_cast<const ast::statement_if &>(stmt));
        break;
      case ast::statement::RETURN:
        vis.visit(static_cast<const ast::statement_return &>(stmt));
        break;
    }
  }

  void visit(const module_item & item, const module_item_visitor & vis) {
    switch(item.variant) {
      case ast::module_item::MEMBER_DECLARATION:
        vis.visit(static_cast<const ast::member_declaration &>(item));
        break;
      case ast::module_item::FUNCTION_DECLARATION:
        vis.visit(static_cast<const ast::function_declaration &>(item));
        break;
      case ast::module_item::FUNCTION_DEFINITION:
        vis.visit(static_cast<const ast::function_definition &>(item));
        break;
    }
  }

  void visit(const file_item & item, const file_item_visitor & vis) {
    switch(item.variant) {
      case ast::file_item::MODULE_ENCLOSURE:
        vis.visit(static_cast<const ast::module_enclosure &>(item));
        break;
      case ast::file_item::STRUCT_DECLARATION:
        vis.visit(static_cast<const ast::struct_declaration &>(item));
        break;
      case ast::file_item::STRUCT_DEFINITION:
        vis.visit(static_cast<const ast::struct_definition &>(item));
        break;
    }
  }
}

