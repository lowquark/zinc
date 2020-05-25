
#include <pp.hpp>

#include <iostream>
#include <sstream>

namespace pp {
  // -----------------------------------------------------------------------------------------------
  // IR

  class rcode_to_string_vis : public ir::rcode_visitor {
    public:
      rcode_to_string_vis(std::string & result)
        : result(result) { }

      virtual void reg(int index) const override {
        std::stringstream ss;
        ss << "r" << index;
        result = ss.str();
      };

      virtual void val(int index) const override {
        std::stringstream ss;
        ss << "$" << index;
        result = ss.str();
      };

      virtual void lvar(int index) const override {
        std::stringstream ss;
        ss << "lv" << index;
        result = ss.str();
      };

      virtual void gvar(int index) const override {
        std::stringstream ss;
        ss << "gv" << index;
        result = ss.str();
      };

    private:
      std::string & result;
  };

  std::string str(ir::rcode code) {
    std::string result;
    ir::visit(code, rcode_to_string_vis(result));
    return result;
  }

  std::string label_name_str(int labelid) {
    std::stringstream ss;
    ss << "L" << labelid;
    return ss.str();
  }

  std::string subroutine_name_str(int subrid) {
    std::stringstream ss;
    ss << "S" << subrid;
    return ss.str();
  }

  class statement_to_string_vis : public ir::statement_visitor {
    public:
      statement_to_string_vis(std::string & result)
        : result(result) { }

      virtual void mov(ir::rcode dst, ir::rcode src) const {
        result = str(dst) + " := " + str(src);
      }

      virtual void neg(ir::rcode dst, ir::rcode src) const {
        result = str(dst) + " := -" + str(src);
      }
      virtual void bnot(ir::rcode dst, ir::rcode src) const {
        result = str(dst) + " := ^" + str(src);
      }
      virtual void lnot(ir::rcode dst, ir::rcode src) const {
        result = str(dst) + " := !" + str(src);
      }

      virtual void add(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " + " + str(src_y);
      }
      virtual void sub(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " - " + str(src_y);
      }
      virtual void mul(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " * " + str(src_y);
      }
      virtual void div(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " / " + str(src_y);
      }

      virtual void lor(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " || " + str(src_y);
      }
      virtual void land(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " && " + str(src_y);
      }

      virtual void ceq(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " == " + str(src_y);
      }
      virtual void cneq(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " != " + str(src_y);
      }
      virtual void clt(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " < " + str(src_y);
      }
      virtual void cleq(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " <= " + str(src_y);
      }
      virtual void cgt(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " > " + str(src_y);
      }
      virtual void cgeq(ir::rcode dst, ir::rcode src_x, ir::rcode src_y) const {
        result = str(dst) + " := " + str(src_x) + " >= " + str(src_y);
      }

      virtual void label(int labelid) const {
        result = ":: " + label_name_str(labelid) + " ::";
      }
      virtual void jmp(int labelid) const {
        result = "goto " + label_name_str(labelid);
      }
      virtual void jz(int labelid, ir::rcode src) const {
        result = "if " + str(src) + " == 0 goto " + label_name_str(labelid);
      }
      virtual void jnz(int labelid, ir::rcode src) const {
        result = "if " + str(src) + " != 0 goto " + label_name_str(labelid);
      }

      virtual void call(int subrid, int num_args, int num_returns) const {
        std::stringstream ss;
        ss << "call " << subroutine_name_str(subrid) <<
              " (" << num_args << ") (" << num_returns << ")";
        result = ss.str();
      }
      virtual void callarg(ir::rcode src) const {
        result = "callarg " + str(src);
      }
      virtual void callres(ir::rcode dst) const {
        result = "callres " + str(dst);
      }

    private:
      std::string & result;
  };

  std::string str(const ir::statement & stmt) {
    std::string result;
    ir::visit(stmt, statement_to_string_vis(result));
    return result;
  }

  void write(std::ostream & os, ir::rcode code) {
    os << "[ir::rcode] " << str(code) << std::endl;
  }

  void write(std::ostream & os, const ir::statement & stmt) {
    os << "[ir::statement] " << str(stmt) << std::endl;
  }

  void write(std::ostream & os, const ir::subroutine & subr) {
    os << "[ir::subroutine]" << std::endl;
    os << subr.name << ":" << std::endl;
    for(auto & stmt : subr.statements) {
      os << "  " << str(stmt) << std::endl;
    }
  }

  void dump(ir::rcode code) {
    write(std::cerr, code);
  }

  void dump(const ir::statement & stmt) {
    write(std::cerr, stmt);
  }

  void dump(const ir::subroutine & subr) {
    write(std::cerr, subr);
  }

  // -----------------------------------------------------------------------------------------------
  // AST

  std::string tabs(int level) {
    return std::string(2*level, ' ');
  }

  std::string str(const ast::name_path & np) {
    std::string s;
    std::size_t n = np.size();
    for(std::size_t i = 0 ; i < n ; ++i) {
      s = s + np[i];
      if(i != n - 1) {
        s = s + ":";
      }
    }
    return s;
  }

  std::string str(const ast::type_specifier & ts) {
    std::string s;
    if(ts.const_qualified) {
      s = s + "const ";
    }
    s = s + str(ts.name);
    if(ts.reference_qualified) {
      s = s + " &";
    }
    return s;
  }

  class write_lvalue_vis : public ast::lvalue_visitor {
    public:
      write_lvalue_vis(std::ostream & os, int level)
        : os(os), indent(tabs(level)), level(level) { }

      virtual void visit(const ast::lvalue_referential & lvalue) const override {
        os << indent << "LVALUE " << lvalue.name << std::endl;
      }

      virtual void visit(const ast::lvalue_declarational & lvalue) const override {
        os << indent << "LVALUE " << str(lvalue.type) << " " << lvalue.name << std::endl;
      }

    private:
      std::ostream & os;
      std::string indent;
      int level;
  };

  class write_expression_vis : public ast::expression_visitor {
    public:
      write_expression_vis(std::ostream & os, int level)
        : os(os), indent(tabs(level)), level(level) { }

      virtual void visit(const ast::expression_integer & expr) const override {
        os << indent << "INTEGER " << expr.value << std::endl;
      }

      virtual void visit(const ast::expression_neg & expr) const override {
        os << indent << "NEG" << std::endl;
        write(os, *expr.subexpression, level + 1);
      }
      virtual void visit(const ast::expression_bnot & expr) const override {
        os << indent << "BNOT" << std::endl;
        write(os, *expr.subexpression, level + 1);
      }
      virtual void visit(const ast::expression_lnot & expr) const override {
        os << indent << "LNOT" << std::endl;
        write(os, *expr.subexpression, level + 1);
      }

      virtual void visit(const ast::expression_add & expr) const override {
        os << indent << "ADD" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_sub & expr) const override {
        os << indent << "SUB" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_mul & expr) const override {
        os << indent << "MUL" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_div & expr) const override {
        os << indent << "DIV" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_land & expr) const override {
        os << indent << "LAND" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_lor & expr) const override {
        os << indent << "LOR" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }

      virtual void visit(const ast::expression_ceq & expr) const override {
        os << indent << "CEQ" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_clt & expr) const override {
        os << indent << "CLT" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_cleq & expr) const override {
        os << indent << "CLEQ" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_cgt & expr) const override {
        os << indent << "CGT" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }
      virtual void visit(const ast::expression_cgeq & expr) const override {
        os << indent << "CGEQ" << std::endl;
        write(os, *expr.subexpression_x, level + 1);
        write(os, *expr.subexpression_y, level + 1);
      }

      virtual void visit(const ast::expression_lvalue & expr) const override {
        os << indent << "LVALUE" << std::endl;
        write(os, *expr.lvalue, level + 1);
      }
      virtual void visit(const ast::expression_call & expr) const override {
        os << indent << "CALL" << std::endl;
        for(auto & expr : expr.arguments) {
          write(os, *expr, level + 1);
        }
      }

    private:
      std::ostream & os;
      std::string indent;
      int level;
  };

  // -----------------------------------------------------------------------------------------------
  // Statements

  class write_statement_vis : public ast::statement_visitor {
    public:
      write_statement_vis(std::ostream & os, int level)
        : os(os), indent(tabs(level)), level(level) { }

      virtual void visit(const ast::statement_assignment & stmt) const override {
        os << indent << "ASSIGNMENT" << std::endl;
        for(auto & lvalue : stmt.lvalues) {
          write(os, *lvalue, level + 1);
        }
        os << indent << ":=" << std::endl;
        for(auto & expr : stmt.expressions) {
          write(os, *expr, level + 1);
        }
      }

      virtual void visit(const ast::statement_block & stmt) const override {
        os << indent << "BLOCK" << std::endl;
        for(auto & sub : stmt.statements) {
          write(os, *sub, level + 1);
        }
      }

      virtual void visit(const ast::statement_call & stmt) const override {
        os << indent << "CALL " << str(stmt.name) << std::endl;
        for(auto & expr : stmt.arguments) {
          write(os, *expr, level + 1);
        }
      }

      virtual void visit(const ast::statement_if & stmt) const override {
        os << indent << "IF" << std::endl;
        write(os, *stmt.condition, level + 1);

        os << indent << "THEN" << std::endl;
        write(os, *stmt.if_true, level + 1);

        if(stmt.if_false) {
          os << indent << "ELSE" << std::endl;
          write(os, *stmt.if_false, level + 1);
        }
      }

      virtual void visit(const ast::statement_return & stmt) const override {
        os << indent << "RETURN" << std::endl;
        for(auto & expr : stmt.expressions) {
          write(os, *expr, level + 1);
        }
      }

    private:
      std::ostream & os;
      std::string indent;
      int level;
  };

  class write_module_item_vis : public ast::module_item_visitor {
    public:
      write_module_item_vis(std::ostream & os, int level)
        : os(os), indent(tabs(level)), level(level) { }

      virtual void visit(const ast::member_declaration & decl) const override {
        os << indent << "MEMBER " << str(decl.type) << " " << decl.name << std::endl;
      }

      virtual void visit(const ast::function_declaration & decl) const override {
        os << indent << "FUNCTION " << decl.name << std::endl;
      }

      virtual void visit(const ast::function_definition & def) const override {
        os << indent << "FUNCTION " << def.name << std::endl;
        for(auto & stmt : def.statements) {
          write(os, *stmt, level + 1);
        }
      }

    private:
      std::ostream & os;
      std::string indent;
      int level;
  };

  class write_file_item_vis : public ast::file_item_visitor {
    public:
      write_file_item_vis(std::ostream & os)
        : os(os) { }

      virtual void visit(const ast::module_enclosure & enc) const override {
        os << "MODULE " << str(enc.name) << std::endl;
        for(auto & item : enc.items) {
          write(os, *item, 1);
        }
      }

      virtual void visit(const ast::struct_declaration & decl) const override {
        os << "STRUCT " << str(decl.name) << std::endl;
      }

      virtual void visit(const ast::struct_definition & def) const override {
        os << "STRUCT " << str(def.name) << std::endl;
      }

    private:
      std::ostream & os;
  };

  void write(std::ostream & os, const ast::lvalue & lvalue, int level) {
    ast::visit(lvalue, write_lvalue_vis(os, level));
  }

  void write(std::ostream & os, const ast::expression & expr, int level) {
    ast::visit(expr, write_expression_vis(os, level));
  }

  void write(std::ostream & os, const ast::statement & stmt, int level) {
    ast::visit(stmt, write_statement_vis(os, level));
  }

  void write(std::ostream & os, const ast::module_item & item, int level) {
    ast::visit(item, write_module_item_vis(os, level));
  }

  void write(std::ostream & os, const ast::file_item & item) {
    ast::visit(item, write_file_item_vis(os));
  }

  void write(std::ostream & os, const ast::file & file) {
    for(auto & item : file.items) {
      write(os, *item);
    }
  }

  void dump(const ast::lvalue & lvalue, int level) {
    write(std::cerr, lvalue, 0);
  }

  void dump(const ast::expression & expr, int level) {
    write(std::cerr, expr, 0);
  }

  void dump(const ast::statement & stmt, int level) {
    write(std::cerr, stmt, 0);
  }

  void dump(const ast::module_item & item, int level) {
    write(std::cerr, item, 0);
  }

  void dump(const ast::file_item & item) {
    write(std::cerr, item);
  }

  void dump(const ast::file & file) {
    write(std::cerr, file);
  }
}

