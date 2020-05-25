
#include <ir.hpp>

namespace ir {
  static constexpr statinfo op_mov     = 0;

  static constexpr statinfo op_neg     = 10;
  static constexpr statinfo op_bnot    = 11;
  static constexpr statinfo op_lnot    = 12;

  static constexpr statinfo op_add     = 20;
  static constexpr statinfo op_sub     = 21;
  static constexpr statinfo op_mul     = 22;
  static constexpr statinfo op_div     = 23;

  static constexpr statinfo op_lor     = 24;
  static constexpr statinfo op_land    = 25;

  static constexpr statinfo op_ceq     = 30;
  static constexpr statinfo op_cneq    = 31;
  static constexpr statinfo op_clt     = 32;
  static constexpr statinfo op_cleq    = 33;
  static constexpr statinfo op_cgt     = 34;
  static constexpr statinfo op_cgeq    = 35;

  static constexpr statinfo op_label   = 40;
  static constexpr statinfo op_jmp     = 41;
  static constexpr statinfo op_jz      = 42;
  static constexpr statinfo op_jnz     = 43;

  static constexpr statinfo op_call    = 50;
  static constexpr statinfo op_callres = 51;
  static constexpr statinfo op_callarg = 52;

  static constexpr rcode rcode_type_mask  = 0xC0000000;
  static constexpr rcode rcode_index_mask = 0x3FFFFFFF;

  static constexpr rcode rcode_type_val  = 0x00000000;
  static constexpr rcode rcode_type_reg  = 0x40000000;
  static constexpr rcode rcode_type_lvar = 0x80000000;
  static constexpr rcode rcode_type_gvar = 0xC0000000;

  static constexpr int rcode_end_index = 0x40000000;

  //------------------------------------------------------------------------------------------------
  // 'rcode' generation

  rcode reg(int index) {
    if(index < 0 || index >= rcode_end_index) {
      throw std::out_of_range("Index out of bounds for rcode");
    }
    return index | rcode_type_reg;
  }

  rcode val(int index) {
    if(index < 0 || index >= rcode_end_index) {
      throw std::out_of_range("Index out of bounds for rcode");
    }
    return index | rcode_type_val;
  }

  rcode lvar(int index) {
    if(index < 0 || index >= rcode_end_index) {
      throw std::out_of_range("Index out of bounds for rcode");
    }
    return index | rcode_type_lvar;
  }

  rcode gvar(int index) {
    if(index < 0 || index >= rcode_end_index) {
      throw std::out_of_range("Index out of bounds for rcode");
    }
    return index | rcode_type_gvar;
  }

  //------------------------------------------------------------------------------------------------
  // 'rcode' reading

  void visit(rcode code, const rcode_visitor & vis) {
    switch(code & rcode_type_mask) {
      case rcode_type_val:
        vis.val(code & rcode_index_mask);
        break;
      case rcode_type_reg:
        vis.reg(code & rcode_index_mask);
        break;
      case rcode_type_lvar:
        vis.lvar(code & rcode_index_mask);
        break;
      case rcode_type_gvar:
        vis.gvar(code & rcode_index_mask);
        break;
    }
  }

  //------------------------------------------------------------------------------------------------
  // Statement usage/modification

  statement mov(rcode dst, rcode src) {
    return statement(op_mov, src, 0, dst);
  }

  statement neg(rcode dst, rcode src) {
    return statement(op_neg, src, 0, dst);
  }

  statement bnot(rcode dst, rcode src) {
    return statement(op_neg, src, 0, dst);
  }

  statement lnot(rcode dst, rcode src) {
    return statement(op_neg, src, 0, dst);
  }

  statement add(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_add, src_x, src_y, dst);
  }

  statement sub(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_sub, src_x, src_y, dst);
  }

  statement mul(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_mul, src_x, src_y, dst);
  }

  statement div(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_div, src_x, src_y, dst);
  }

  statement lor(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_lor, src_x, src_y, dst);
  }

  statement land(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_land, src_x, src_y, dst);
  }

  statement ceq(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_ceq, src_x, src_y, dst);
  }

  statement cneq(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_cneq, src_x, src_y, dst);
  }

  statement clt(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_clt, src_x, src_y, dst);
  }

  statement cleq(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_cleq, src_x, src_y, dst);
  }

  statement cgt(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_cgt, src_x, src_y, dst);
  }

  statement cgeq(rcode dst, rcode src_x, rcode src_y) {
    return statement(op_cgeq, src_x, src_y, dst);
  }

  statement label(int labelid) {
    return statement(op_label, labelid, 0, 0);
  }

  statement jmp(int labelid) {
    return statement(op_jmp, 0, 0, labelid);
  }

  statement jz(int labelid, rcode src) {
    return statement(op_jz, src, 0, labelid);
  }

  statement jnz(int labelid, rcode src) {
    return statement(op_jnz, src, 0, labelid);
  }

  statement call(int subroutineid, int num_args, int num_returns) {
    return statement(op_call, num_args, num_returns, subroutineid);
  }

  statement callarg(rcode src) {
    return statement(op_callarg, src, 0, 0);
  }

  statement callres(rcode dst) {
    return statement(op_callres, 0, 0, dst);
  }

  void visit(const statement & stmt, const statement_visitor & vis) {
    switch(stmt.info) {
      case op_mov:
        vis.mov(stmt.z, stmt.x);
        break;

      case op_neg:
        vis.neg(stmt.z, stmt.x);
        break;
      case op_bnot:
        vis.bnot(stmt.z, stmt.x);
        break;
      case op_lnot:
        vis.lnot(stmt.z, stmt.x);
        break;

      case op_add:
        vis.add(stmt.z, stmt.x, stmt.y);
        break;
      case op_sub:
        vis.sub(stmt.z, stmt.x, stmt.y);
        break;
      case op_mul:
        vis.mul(stmt.z, stmt.x, stmt.y);
        break;
      case op_div:
        vis.div(stmt.z, stmt.x, stmt.y);
        break;

      case op_lor:
        vis.lor(stmt.z, stmt.x, stmt.y);
        break;
      case op_land:
        vis.land(stmt.z, stmt.x, stmt.y);
        break;

      case op_ceq:
        vis.ceq(stmt.z, stmt.x, stmt.y);
        break;
      case op_cneq:
        vis.cneq(stmt.z, stmt.x, stmt.y);
        break;
      case op_clt:
        vis.clt(stmt.z, stmt.x, stmt.y);
        break;
      case op_cleq:
        vis.cleq(stmt.z, stmt.x, stmt.y);
        break;
      case op_cgt:
        vis.cgt(stmt.z, stmt.x, stmt.y);
        break;
      case op_cgeq:
        vis.cgeq(stmt.z, stmt.x, stmt.y);
        break;

      case op_label:
        vis.label(stmt.x);
        break;
      case op_jmp:
        vis.jmp(stmt.z);
        break;
      case op_jz:
        vis.jz(stmt.z, stmt.x);
        break;
      case op_jnz:
        vis.jnz(stmt.z, stmt.x);
        break;

      case op_call:
        vis.call(stmt.z, stmt.x, stmt.y);
        break;
      case op_callarg:
        vis.callarg(stmt.x);
        break;
      case op_callres:
        vis.callres(stmt.z);
        break;
    }
  }

  //------------------------------------------------------------------------------------------------
  // Program generation

  void append_statement(subroutine & subr, const statement & stmt) {
    subr.statements.push_back(stmt);
  }

  rcode argument_reg(const subroutine & subr, int index) {
    if(index < 0 || index >= subr.num_arguments) {
      throw std::out_of_range("Index out of bounds for argument register");
    }
    return reg(index);
  }

  rcode return_reg(const subroutine & subr, int index) {
    if(index < 0 || index >= subr.num_returns) {
      throw std::out_of_range("Index out of bounds for return register");
    }
    return reg(index + subr.num_arguments);
  }

  rcode temporary_reg(const subroutine & subr, int index) {
    return reg(index + subr.num_returns + subr.num_arguments);
  }
}

