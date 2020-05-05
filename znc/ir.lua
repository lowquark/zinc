
----------------------------------------------------------------------------------------------------
-- IR - (I)ntermediate (R)epresentation
----------------------------------------------------------------------------------------------------

local function deepcopy(t)
  r = { }
  for k,v in pairs(t) do
    if v.type == 'table' then
      r[k] = deepcopy(v)
    else
      r[k] = v
    end
  end
  return r
end

----------------------------------------------------------------------------------------------------
-- Program construction

-- Unless otherwise specified, table values are copied

local ir = {}

function ir.mov(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'mov',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.neg(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'neg',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.bnot(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'bnot',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.lnot(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'lnot',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.add(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'add',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.sub(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'sub',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.mul(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'mul',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.div(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'div',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.eq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'eq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.neq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'neq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.lt(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'lt',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.gt(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'gt',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.leq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'leq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.geq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'geq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.salloc(size)
  assert(size)
  return { type = 'salloc', size = size }
end

function ir.sfree(size)
  assert(size)
  return { type = 'sfree', size = size }
end

function ir.call(return_regs, name, argument_regs)
  assert(return_regs)
  assert(name)
  assert(argument_regs)
  return { type = 'call',
           argument_regs = deepcopy(argument_regs),
           name = name,
           return_regs = deepcopy(return_regs) }
end

function ir.ret()
  return { type = 'ret' }
end

-- Defines a jump label
function ir.label(name)
  assert(name)
  return { type = 'label', name = name }
end

-- Unconditional jump
function ir.jmp(label_name)
  assert(label_name)
  return { type = 'jmp', label_name = label_name }
end

-- Jump if equal
function ir.jeq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jeq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if not equal
function ir.jneq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jneq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if zero
function ir.jz(label_name, reg)
  assert(label_name)
  assert(reg)
  return { type = 'jz',
           label_name = label_name,
           register_x = reg }
end

-- Jump if nonzero
function ir.jnz(label_name, reg)
  assert(label_name)
  assert(reg)
  return { type = 'jnz',
           label_name = label_name,
           register_x = reg }
end

-- Jump if less than
function ir.jlt(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jlt',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if greater than
function ir.jgt(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jgt',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if less than or equal
function ir.jleq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jleq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if greater than or equal
function ir.jgeq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jgeq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.subroutine(args, rets, stmts)
  assert(args)
  assert(rets)
  assert(stmts)
  return { type = 'subroutine',
           arguments = deepcopy(args),
           returns = deepcopy(rets),
           statements = deepcopy(stmts) }
end

----------------------------------------------------------------------------------------------------
-- Pretty printing

-- (s)atement (s)tring (t)able
local sst = {}

function sst.mov(stmt)
  return stmt.register_z..' := '..stmt.register_x
end

function sst.neg(stmt)
  return stmt.register_z..' := -'..stmt.register_x
end

function sst.bnot(stmt)
  return stmt.register_z..' := ~'..stmt.register_x
end

function sst.lnot(stmt)
  return stmt.register_z..' := !'..stmt.register_x
end

function sst.add(stmt)
  return stmt.register_z..' := '..stmt.register_x..' + '..stmt.register_y
end

function sst.sub(stmt)
  return stmt.register_z..' := '..stmt.register_x..' - '..stmt.register_y
end

function sst.mul(stmt)
  return stmt.register_z..' := '..stmt.register_x..' * '..stmt.register_y
end

function sst.div(stmt)
  return stmt.register_z..' := '..stmt.register_x..' / '..stmt.register_y
end

function sst.eq(stmt)
  return stmt.register_z..' := '..stmt.register_x..' == '..stmt.register_y
end

function sst.neq(stmt)
  return stmt.register_z..' := '..stmt.register_x..' != '..stmt.register_y
end

function sst.lt(stmt)
  return stmt.register_z..' := '..stmt.register_x..' < '..stmt.register_y
end

function sst.gt(stmt)
  return stmt.register_z..' := '..stmt.register_x..' > '..stmt.register_y
end

function sst.leq(stmt)
  return stmt.register_z..' := '..stmt.register_x..' <= '..stmt.register_y
end

function sst.geq(stmt)
  return stmt.register_z..' := '..stmt.register_x..' >= '..stmt.register_y
end

function sst.call(stmt)
  local str = ''
  if #stmt.return_regs > 0 then
    str = '('
    for i,return_reg in ipairs(stmt.return_regs) do
      if i == 1 then
        str = str..return_reg
      else
        str = str..', '..return_reg
      end
    end
    str = str..') := '
  end
  str = str..stmt.name..' ('
  for i,argument_reg in ipairs(stmt.argument_regs) do
    if i == 1 then
      str = str..argument_reg
    else
      str = str..', '..argument_reg
    end
  end
  str = str..')'
  return str
end

function sst.ret(stmt)
  return 'ret'
end

function sst.label(stmt)
  return stmt.name..':'
end

function sst.jmp(stmt)
  return 'goto '..stmt.label_name
end

function sst.jz(stmt)
  return 'if !'..stmt.register_x..' goto '..stmt.label_name
end

function sst.jnz(stmt)
  return 'if '..stmt.register_x..' goto '..stmt.label_name
end

function sst.jeq(stmt)
  return 'if '..stmt.register_x..' == '..stmt.register_y..' goto '..stmt.label_name
end

function sst.jneq(stmt)
  return 'if '..stmt.register_x..' != '..stmt.register_y..' goto '..stmt.label_name
end

function sst.jlt(stmt)
  return 'if '..stmt.register_x..' < '..stmt.register_y..' goto '..stmt.label_name
end

function sst.jgt(stmt)
  return 'if '..stmt.register_x..' > '..stmt.register_y..' goto '..stmt.label_name
end

function sst.jleq(stmt)
  return 'if '..stmt.register_x..' <= '..stmt.register_y..' goto '..stmt.label_name
end

function sst.jgeq(stmt)
  return 'if '..stmt.register_x..' >= '..stmt.register_y..' goto '..stmt.label_name
end

local function stmt_string(stmt)
  local h = sst[stmt.type]
  if h then
    return h(stmt)
  else
    error('Unknown IR statement type `'..tostring(stmt.type)..'`')
  end
end

local function dump_statement(stmt)
  if stmt.type == 'label' then
    io.write(stmt_string(stmt)..'\n')
  else
    io.write('  '..stmt_string(stmt)..'\n')
  end
end

function ir.dump_subroutine(subr)
  io.write('sub '..subr.name..' (')
  for i,arg_type in ipairs(subr.arguments) do
    io.write(arg_type)
    if i ~= #subr.arguments then
      io.write(',')
    end
  end
  io.write(' : ')
  for i,arg_type in ipairs(subr.returns) do
    io.write(arg_type)
    if i ~= #subr.returns then
      io.write(',')
    end
  end
  io.write(')\n')
  for i,stmt in ipairs(subr.statements) do
    dump_statement(stmt)
  end
end

-- Pretty-prints the given IR program object to io.output
function ir.dump(ir_prog)
  for i,subr in ipairs(ir_prog.subroutines) do
    ir.dump_subroutine(subr)
  end
end

ir.statement_string = stmt_string

return ir

