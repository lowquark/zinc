#ifndef IR_HPP
#define IR_HPP

#include <cstdint>
#include <stdexcept>
#include <vector>

namespace ir {
  // Statement info field
  typedef std::uint32_t statinfo;

  // Statement operand codes
  typedef std::uint32_t rcode;

  // IR statements
  class statement {
    public:
      statinfo info = 0;
      rcode x = 0;
      rcode y = 0;
      rcode z = 0;

      statement() = default;
      statement(statinfo info, rcode x, rcode y, rcode z)
        : info(info), x(x), y(y), z(z) { }
  };

  // IR subroutines
  class subroutine {
    public:
      std::string name;
      std::vector<statement> statements;
      int num_registers;
      int numals;
      int num_arguments;
      int num_returns;
  };

  // IR programs
  class program {
    public:
      std::vector<subroutine> subroutines;
  };

  //------------------------------------------------------------------------------------------------
  // 'rcode' generation

  rcode reg(int index);
  rcode val(int index);
  rcode lvar(int index);
  rcode gvar(int index);

  //------------------------------------------------------------------------------------------------
  // 'rcode' reading

  class rcode_visitor {
    public:
      virtual void reg(int index) const { };
      virtual void val(int index) const { };
      virtual void lvar(int index) const { };
      virtual void gvar(int index) const { };
  };

  void visit(rcode code, const rcode_visitor & vis);

  //------------------------------------------------------------------------------------------------
  // Statement generation

  statement mov(rcode dst, rcode src);

  // Unary operators
  statement neg(rcode dst, rcode src);
  statement bnot(rcode dst, rcode src);
  statement lnot(rcode dst, rcode src);

  // Binary operators
  statement add(rcode dst, rcode src_x, rcode src_y);
  statement sub(rcode dst, rcode src_x, rcode src_y);
  statement mul(rcode dst, rcode src_x, rcode src_y);
  statement div(rcode dst, rcode src_x, rcode src_y);

  statement lor(rcode dst, rcode src_x, rcode src_y);
  statement land(rcode dst, rcode src_x, rcode src_y);

  statement ceq(rcode dst, rcode src_x, rcode src_y);
  statement cneq(rcode dst, rcode src_x, rcode src_y);
  statement clt(rcode dst, rcode src_x, rcode src_y);
  statement cleq(rcode dst, rcode src_x, rcode src_y);
  statement cgt(rcode dst, rcode src_x, rcode src_y);
  statement cgeq(rcode dst, rcode src_x, rcode src_y);

  // Jumping
  statement label(int labelid);
  statement jmp(int labelid);
  statement jz(int labelid, rcode src);
  statement jnz(int labelid, rcode src);

  // Function calls
  statement call(int subroutineid, int num_args, int num_returns);
  statement callarg(rcode code);
  statement callres(rcode code);

  //------------------------------------------------------------------------------------------------
  // Statement reading

  class statement_visitor {
    public:
      virtual void mov  (rcode dst, rcode src) const { };

      virtual void neg  (rcode dst, rcode src) const { };
      virtual void bnot (rcode dst, rcode src) const { };
      virtual void lnot (rcode dst, rcode src) const { };

      virtual void add  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void sub  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void mul  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void div  (rcode dst, rcode src_x, rcode src_y) const { };

      virtual void lor  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void land (rcode dst, rcode src_x, rcode src_y) const { };

      virtual void ceq  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void cneq (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void clt  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void cleq (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void cgt  (rcode dst, rcode src_x, rcode src_y) const { };
      virtual void cgeq (rcode dst, rcode src_x, rcode src_y) const { };

      virtual void label (int labelid) const { };
      virtual void jmp   (int labelid) const { };
      virtual void jz    (int labelid, rcode src) const { };
      virtual void jnz   (int labelid, rcode src) const { };

      virtual void call    (int subroutineid, int num_args, int num_returns) const { };
      virtual void callarg (rcode src) const { };
      virtual void callres (rcode dst) const { };
  };

  void visit(const statement & stmt, const statement_visitor & vis);

  //------------------------------------------------------------------------------------------------
  // Subroutine generation

  void append_statement(subroutine & subr, const statement & stmt);

  rcode argument_reg(const subroutine & subr, int index);
  rcode return_reg(const subroutine & subr, int index);
  rcode temporary_reg(const subroutine & subr, int index);
}

#endif
