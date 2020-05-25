#ifndef PP_H
#define PP_H

#include <ir.hpp>
#include <ast.hpp>

namespace pp {
  // -----------------------------------------------------------------------------------------------
  // IR

  std::string str(ir::rcode code);
  std::string str(const ir::statement & stmt);

  void write(std::ostream & os, ir::rcode code);
  void write(std::ostream & os, const ir::statement & stmt);
  void write(std::ostream & os, const ir::subroutine & subr);

  void dump(ir::rcode code);
  void dump(const ir::statement & stmt);
  void dump(const ir::subroutine & subr);

  // -----------------------------------------------------------------------------------------------
  // AST

  std::string str(const ast::name_path & np);
  std::string str(const ast::type_specifier & ts);

  void write(std::ostream & os, const ast::lvalue & lvalue, int level);
  void write(std::ostream & os, const ast::expression & expr, int level);
  void write(std::ostream & os, const ast::statement & stmt, int level);
  void write(std::ostream & os, const ast::module_item & item, int level);
  void write(std::ostream & os, const ast::file_item & item);
  void write(std::ostream & os, const ast::file & file);

  void dump(const ast::lvalue & lvalue, int level);
  void dump(const ast::expression & expr, int level);
  void dump(const ast::statement & stmt, int level);
  void dump(const ast::module_item & item, int level);
  void dump(const ast::file_item & item);
  void dump(const ast::file & file);
}

#endif
