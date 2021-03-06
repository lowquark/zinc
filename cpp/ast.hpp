#ifndef AST_HPP
#define AST_HPP

#include <memory>
#include <string>
#include <vector>

// Random note: Field and array indexing
// ------------------------
//
// Example:
//   1) a.b.c
//   2) a.b.c[i]
//   3) a.b.c[i].d
//
// Procedure:
//   1) a.b.c starts with a's variable address (say, lv0) and adds a calculated offset
//     -> Evaluation should return a simple "register" lvalue reference
//     -> Assignment is a simple mov
//     -> Reference is simple register usage
//   2) a.b.c[i] 
//     -> Evaluation should return a register / offset lvalue reference
//     -> Assignment is an array assignment
//     -> Reference is an array reference instruction... into a temporary?
//   3) a.b.c[i].d
//     -> Evaluation should return the same as (2), but with a larger initial offset
//     -> Assignment is an array assignment
//     -> Reference is an array reference instruction... into a temporary?
//   But now it needs to be a pointer... stored in a temporary?
//   I guess lvalues sometimes evaluate to pointers.
//
//   3) a.b.c[i].d->m
//     dst_addr = &a;
//     dst_addr += offsetof(A::b);
//     dst_addr += offsetof(B::c);
//     dst_addr += i*elemsize(C);
//     dst_addr += offsetof(elemtype(C)::d);
//     dst_addr = *dst_addr;

namespace ast {
  using std::string;
  using std::unique_ptr;
  using std::vector;

  // -----------------------------------------------------------------------------------------------
  // Misc.

  typedef vector<string> name_path;

  struct type_specifier {
    bool const_qualified;
    bool reference_qualified;
    ast::name_path name_path;
  };

  // -----------------------------------------------------------------------------------------------
  // Lvalues

  struct lvalue {
    enum variantid {
      DECLARATIONAL,
      REFERENTIAL,
    };

    lvalue(variantid v) : variant(v) { }

    const variantid variant;
  };

  typedef vector<unique_ptr<lvalue>> lvalue_list;

  struct lvalue_declarational : public lvalue {
    lvalue_declarational() : lvalue(DECLARATIONAL) { }
    unique_ptr<type_specifier> type;
    string name;
  };

  struct lvalue_referential : public lvalue {
    lvalue_referential() : lvalue(REFERENTIAL) { }
    string name;
  };

  class lvalue_visitor {
    public:
      virtual void visit(const lvalue_declarational & expr) const { }
      virtual void visit(const lvalue_referential & expr) const { }
  };

  void visit(const lvalue & expr, const lvalue_visitor & vis);

  // -----------------------------------------------------------------------------------------------
  // Expressions

  struct expression {
    enum variantid {
      INTEGER,
      NEG,
      BNOT,
      LNOT,
      ADD,
      SUB,
      MUL,
      DIV,
      LAND,
      LOR,
      CEQ,
      CNEQ,
      CLT,
      CLEQ,
      CGT,
      CGEQ,
      LVALUE,
      CALL,
    };

    expression(variantid v) : variant(v) { }

    const variantid variant;
  };

  typedef vector<unique_ptr<expression>> expression_list;

  struct expression_unary_operator : public expression {
    expression_unary_operator(variantid v) : expression(v) { }
    expression_unary_operator(variantid v, unique_ptr<expression> && subexpr)
      : expression(v), subexpression(std::move(subexpr)) { }
    unique_ptr<expression> subexpression;
  };

  struct expression_binary_operator : public expression {
    expression_binary_operator(variantid v) : expression(v) { }
    expression_binary_operator(variantid v,
                               unique_ptr<expression> && subexpr_x,
                               unique_ptr<expression> && subexpr_y)
      : expression(v),
        subexpression_x(std::move(subexpr_x)),
        subexpression_y(std::move(subexpr_y)) { }
    unique_ptr<expression> subexpression_x;
    unique_ptr<expression> subexpression_y;
  };

  struct expression_integer : public expression {
    int64_t value;

    expression_integer() : expression(INTEGER) { }
    expression_integer(int64_t value) : expression(INTEGER), value(value) { }
  };

  struct expression_neg : public expression_unary_operator {
    expression_neg() : expression_unary_operator(NEG) { }
    expression_neg(unique_ptr<expression> && expr)
      : expression_unary_operator(NEG, std::move(expr)) { }
  };

  struct expression_bnot : public expression_unary_operator {
    expression_bnot() : expression_unary_operator(BNOT) { }
    expression_bnot(unique_ptr<expression> && expr)
      : expression_unary_operator(BNOT, std::move(expr)) { }
  };

  struct expression_lnot : public expression_unary_operator {
    expression_lnot() : expression_unary_operator(LNOT) { }
    expression_lnot(unique_ptr<expression> && expr)
      : expression_unary_operator(LNOT, std::move(expr)) { }
  };

  struct expression_add : public expression_binary_operator {
    expression_add() : expression_binary_operator(ADD) { }
    expression_add(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(ADD, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_sub : public expression_binary_operator {
    expression_sub() : expression_binary_operator(SUB) { }
    expression_sub(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(SUB, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_mul : public expression_binary_operator {
    expression_mul() : expression_binary_operator(MUL) { }
    expression_mul(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(MUL, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_div : public expression_binary_operator {
    expression_div() : expression_binary_operator(DIV) { }
    expression_div(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(DIV, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_land : public expression_binary_operator {
    expression_land() : expression_binary_operator(LAND) { }
    expression_land(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(LAND, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_lor : public expression_binary_operator {
    expression_lor() : expression_binary_operator(LOR) { }
    expression_lor(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(LOR, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_ceq : public expression_binary_operator {
    expression_ceq() : expression_binary_operator(CEQ) { }
    expression_ceq(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(CEQ, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_cneq : public expression_binary_operator {
    expression_cneq() : expression_binary_operator(CNEQ) { }
    expression_cneq(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(CNEQ, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_clt : public expression_binary_operator {
    expression_clt() : expression_binary_operator(CLT) { }
    expression_clt(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(CLT, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_cleq : public expression_binary_operator {
    expression_cleq() : expression_binary_operator(CLEQ) { }
    expression_cleq(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(CLEQ, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_cgt : public expression_binary_operator {
    expression_cgt() : expression_binary_operator(CGT) { }
    expression_cgt(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(CGT, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_cgeq : public expression_binary_operator {
    expression_cgeq() : expression_binary_operator(CGEQ) { }
    expression_cgeq(unique_ptr<expression> && expr_x, unique_ptr<expression> && expr_y)
      : expression_binary_operator(CGEQ, std::move(expr_x), std::move(expr_y)) { }
  };

  struct expression_lvalue : public expression {
    expression_lvalue() : expression(LVALUE) { }
    expression_lvalue(unique_ptr<ast::lvalue> && lvalue)
      : expression(LVALUE), lvalue(std::move(lvalue)) { }

    unique_ptr<ast::lvalue> lvalue;
  };

  struct expression_call : public expression {
    expression_call() : expression(CALL) { }

    ast::name_path name_path;
    expression_list arguments;
  };

  class expression_visitor {
    public:
      virtual void visit(const expression_integer & expr) const { }

      virtual void visit(const expression_neg & expr) const { }
      virtual void visit(const expression_bnot & expr) const { }
      virtual void visit(const expression_lnot & expr) const { }

      virtual void visit(const expression_add & expr) const { }
      virtual void visit(const expression_sub & expr) const { }
      virtual void visit(const expression_mul & expr) const { }
      virtual void visit(const expression_div & expr) const { }
      virtual void visit(const expression_land & expr) const { }
      virtual void visit(const expression_lor & expr) const { }

      virtual void visit(const expression_ceq & expr) const { }
      virtual void visit(const expression_cneq & expr) const { }
      virtual void visit(const expression_clt & expr) const { }
      virtual void visit(const expression_cleq & expr) const { }
      virtual void visit(const expression_cgt & expr) const { }
      virtual void visit(const expression_cgeq & expr) const { }

      virtual void visit(const expression_lvalue & expr) const { }
      virtual void visit(const expression_call & expr) const { }
  };

  void visit(const expression & expr, const expression_visitor & vis);

  // -----------------------------------------------------------------------------------------------
  // Statements

  struct statement {
    enum variantid {
      ASSIGNMENT,
      BLOCK,
      CALL,
      IF,
      RETURN,
    };

    statement(variantid v) : variant(v) { }

    const variantid variant;
  };

  typedef vector<unique_ptr<statement>> statement_list;

  struct statement_assignment : public statement {
    statement_assignment() : statement(ASSIGNMENT) { }

    lvalue_list lvalues;
    expression_list expressions;
  };

  struct statement_block : public statement {
    statement_block() : statement(BLOCK) { }

    statement_list statements;
  };

  struct statement_call : public statement {
    statement_call() : statement(CALL) { }

    ast::name_path name_path;
    expression_list arguments;
  };

  struct statement_if : public statement {
    statement_if() : statement(IF) { }

    unique_ptr<expression> condition;
    unique_ptr<statement> if_block;
    unique_ptr<statement> else_block;
  };

  struct statement_return : public statement {
    statement_return() : statement(RETURN) { }

    expression_list expressions;
  };

  class statement_visitor {
    public:
      virtual void visit(const statement_assignment & stmt) const { }
      virtual void visit(const statement_block & stmt) const { }
      virtual void visit(const statement_call & stmt) const { }
      virtual void visit(const statement_if & stmt) const { }
      virtual void visit(const statement_return & stmt) const { }
  };

  void visit(const statement & stmt, const statement_visitor & vis);

  // -----------------------------------------------------------------------------------------------
  // Function stuff

  struct function_argument {
    unique_ptr<type_specifier> type;
    string name;
  };

  struct function_return {
    unique_ptr<type_specifier> type;
  };

  // -----------------------------------------------------------------------------------------------
  // Module items

  struct module_item {
    enum variantid {
      MEMBER_DECLARATION,
      FUNCTION_DECLARATION,
      FUNCTION_DEFINITION,
    };

    module_item(variantid v) : variant(v) { }

    const variantid variant;
  };

  typedef vector<unique_ptr<module_item>> module_item_list;

  struct member_declaration : public module_item {
    member_declaration() : module_item(MEMBER_DECLARATION) { }

    unique_ptr<type_specifier> type;
    string name;
  };

  struct function_declaration : public module_item {
    function_declaration() : module_item(FUNCTION_DECLARATION) { }

    string name;
    vector<function_argument> arguments;
    vector<function_return> returns;
  };

  struct function_definition : public module_item {
    function_definition() : module_item(FUNCTION_DEFINITION) { }

    string name;
    vector<function_argument> arguments;
    vector<function_return> returns;
    unique_ptr<statement_block> block;
  };

  class module_item_visitor {
    public:
      virtual void visit(const member_declaration & decl) const { }
      virtual void visit(const function_declaration & decl) const { }
      virtual void visit(const function_definition & def) const { }
  };

  void visit(const module_item & mi, const module_item_visitor & vis);

  // -----------------------------------------------------------------------------------------------
  // File items

  struct file_item {
    enum variantid {
      MODULE_ENCLOSURE,
      STRUCT_DECLARATION,
      STRUCT_DEFINITION,
    };

    file_item(variantid v) : variant(v) { }

    const variantid variant;
  };

  typedef vector<unique_ptr<file_item>> file_item_list;

  struct module_enclosure : public file_item {
    module_enclosure() : file_item(MODULE_ENCLOSURE) { }

    ast::name_path name_path;
    module_item_list items;
  };

  struct struct_declaration : public file_item {
    struct_declaration() : file_item(STRUCT_DECLARATION) { }
    ast::name_path name_path;
  };

  struct struct_definition : public file_item {
    struct_definition() : file_item(STRUCT_DEFINITION) { }
    ast::name_path name_path;
  };

  class file_item_visitor {
    public:
      virtual void visit(const module_enclosure & enc) const { }
      virtual void visit(const struct_declaration & decl) const { }
      virtual void visit(const struct_definition & def) const { }
  };

  void visit(const file_item & item, const file_item_visitor & vis);

  // -----------------------------------------------------------------------------------------------
  // File
  //
  // Top level ast node

  struct file {
    file_item_list items;
  };
}

#endif
