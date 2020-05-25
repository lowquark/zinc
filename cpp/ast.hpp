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
    bool reference_qualified;
    bool const_qualified;
    name_path name;
  };

  // -----------------------------------------------------------------------------------------------
  // Lvalues

  // Declarations must be lvalues, in order to support multiple assignment
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
    type_specifier type;
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
    unique_ptr<expression> subexpression;
  };

  struct expression_binary_operator : public expression {
    expression_binary_operator(variantid v) : expression(v) { }
    unique_ptr<expression> subexpression_x;
    unique_ptr<expression> subexpression_y;
  };

  struct expression_integer : public expression {
    int64_t value;

    expression_integer() : expression(INTEGER) { }
  };

  struct expression_neg : public expression_unary_operator {
    expression_neg() : expression_unary_operator(NEG) { }
  };

  struct expression_bnot : public expression_unary_operator {
    expression_bnot() : expression_unary_operator(BNOT) { }
  };

  struct expression_lnot : public expression_unary_operator {
    expression_lnot() : expression_unary_operator(LNOT) { }
  };

  struct expression_add : public expression_binary_operator {
    expression_add() : expression_binary_operator(ADD) { }
  };

  struct expression_sub : public expression_binary_operator {
    expression_sub() : expression_binary_operator(SUB) { }
  };

  struct expression_mul : public expression_binary_operator {
    expression_mul() : expression_binary_operator(MUL) { }
  };

  struct expression_div : public expression_binary_operator {
    expression_div() : expression_binary_operator(DIV) { }
  };

  struct expression_land : public expression_binary_operator {
    expression_land() : expression_binary_operator(LAND) { }
  };

  struct expression_lor : public expression_binary_operator {
    expression_lor() : expression_binary_operator(LOR) { }
  };

  struct expression_ceq : public expression_binary_operator {
    expression_ceq() : expression_binary_operator(CEQ) { }
  };

  struct expression_clt : public expression_binary_operator {
    expression_clt() : expression_binary_operator(CLT) { }
  };

  struct expression_cleq : public expression_binary_operator {
    expression_cleq() : expression_binary_operator(CLEQ) { }
  };

  struct expression_cgt : public expression_binary_operator {
    expression_cgt() : expression_binary_operator(CGT) { }
  };

  struct expression_cgeq : public expression_binary_operator {
    expression_cgeq() : expression_binary_operator(CGEQ) { }
  };

  struct expression_lvalue : public expression {
    expression_lvalue() : expression(LVALUE) { }

    unique_ptr<ast::lvalue> lvalue;
  };

  struct expression_call : public expression {
    expression_call() : expression(CALL) { }

    name_path name;
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

    name_path name;
    expression_list arguments;
  };

  struct statement_if : public statement {
    statement_if() : statement(IF) { }

    unique_ptr<expression> condition;
    unique_ptr<statement> if_true;
    unique_ptr<statement> if_false;
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

    type_specifier type;
    string name;
  };

  struct function_declaration : public module_item {
    function_declaration() : module_item(FUNCTION_DECLARATION) { }

    string name;
  };

  struct function_definition : public module_item {
    function_definition() : module_item(FUNCTION_DEFINITION) { }

    string name;
    statement_list statements;
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

    name_path name;
    module_item_list items;
  };

  struct struct_declaration : public file_item {
    struct_declaration() : file_item(STRUCT_DECLARATION) { }
    name_path name;
  };

  struct struct_definition : public file_item {
    struct_definition() : file_item(STRUCT_DEFINITION) { }
    name_path name;
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
