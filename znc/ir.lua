
----------------------------------------------------------------------------------------------------
-- IR - (I)ntermediate (R)epresentation
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Tree construction

local ir = {}

function ir.mov(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'mov', register_z = reg_z, register_x = reg_x }
end

function ir.neg(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'neg', register_z = reg_z, register_x = reg_x }
end
function ir.bnot(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'bnot', register_z = reg_z, register_x = reg_x }
end
function ir.lnot(reg_z, reg_x)
  assert(reg_z)
  assert(reg_x)
  return { type = 'lnot', register_z = reg_z, register_x = reg_x }
end

function ir.add(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'add', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.sub(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'sub', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.mul(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'mul', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.div(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'div', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end

function ir.eq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'eq', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.neq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'neq', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.lt(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'lt', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.gt(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'gt', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.leq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'leq', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end
function ir.geq(reg_z, reg_x, reg_y)
  assert(reg_z)
  assert(reg_x)
  assert(reg_y)
  return { type = 'geq', register_z = reg_z, register_x = reg_x, register_y = reg_y }
end

function ir.salloc(size)
  assert(size)
  return { type = 'salloc', size = size }
end
function ir.sfree(size)
  assert(size)
  return { type = 'sfree', size = size }
end

function ir.begincall(size)
  assert(size)
  return { type = 'begincall', size = size }
end
function ir.endcall()
  return { type = 'endcall' }
end
function ir.call(name)
  assert(name)
  return { type = 'call', name = name }
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
  return { type = 'jeq', label_name = label_name, register_x = reg_x, register_y = reg_y }
end
-- Jump if not equal
function ir.jneq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jneq', label_name = label_name, register_x = reg_x, register_y = reg_y }
end
-- Jump if zero
function ir.jz(label_name, reg)
  assert(label_name)
  assert(reg)
  return { type = 'jz', label_name = label_name, register = reg }
end
-- Jump if nonzero
function ir.jnz(label_name, reg)
  assert(label_name)
  assert(reg)
  return { type = 'jnz', label_name = label_name, register = reg }
end
-- Jump if less than
function ir.jlt(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jlt', label_name = label_name, register_x = reg_x, register_y = reg_y }
end
-- Jump if greater than
function ir.jgt(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jgt', label_name = label_name, register_x = reg_x, register_y = reg_y }
end
-- Jump if less than or equal
function ir.jleq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jleq', label_name = label_name, register_x = reg_x, register_y = reg_y }
end
-- Jump if greater than or equal
function ir.jgeq(label_name, reg_x, reg_y)
  assert(label_name)
  assert(reg_x)
  assert(reg_y)
  return { type = 'jgeq', label_name = label_name, register_x = reg_x, register_y = reg_y }
end

function ir.subroutine(args, rets, stmts, meta)
  assert(args)
  assert(rets)
  assert(stmts)
  return { type = 'subroutine', arguments = args, returns = rets, statements = stmts, meta = meta or { } }
end

----------------------------------------------------------------------------------------------------
-- Pretty printing

-- (d)ump (s)tatement (t)able
local dst = {}

function dst.salloc(stmt)
  io.write('  salloc '..stmt.size..'\n')
end
function dst.sfree(stmt)
  io.write('  sfree '..stmt.size..'\n')
end

function dst.mov(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..'\n')
end

function dst.neg(stmt)
  io.write('  '..stmt.register_z..' := -'..stmt.register_x..'\n')
end
function dst.bnot(stmt)
  io.write('  '..stmt.register_z..' := ~'..stmt.register_x..'\n')
end
function dst.lnot(stmt)
  io.write('  '..stmt.register_z..' := !'..stmt.register_x..'\n')
end

function dst.add(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' + '..stmt.register_y..'\n')
end
function dst.sub(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' - '..stmt.register_y..'\n')
end
function dst.mul(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' * '..stmt.register_y..'\n')
end
function dst.div(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' / '..stmt.register_y..'\n')
end

function dst.eq(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' == '..stmt.register_y..'\n')
end
function dst.neq(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' != '..stmt.register_y..'\n')
end
function dst.lt(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' < '..stmt.register_y..'\n')
end
function dst.gt(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' > '..stmt.register_y..'\n')
end
function dst.leq(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' <= '..stmt.register_y..'\n')
end
function dst.geq(stmt)
  io.write('  '..stmt.register_z..' := '..stmt.register_x..' >= '..stmt.register_y..'\n')
end

function dst.begincall(stmt)
  io.write('  begincall '..stmt.size..'\n')
end
function dst.endcall(stmt)
  io.write('  endcall\n')
end
function dst.call(stmt)
  io.write('  call '..stmt.name..'\n')
end
function dst.ret(stmt)
  io.write('  ret\n')
end

function dst.label(stmt)
  io.write(stmt.name..':\n')
end
function dst.jmp(stmt)
  io.write('  goto '..stmt.label_name..'\n')
end
function dst.jz(stmt)
  io.write('  if !'..stmt.register..' goto '..stmt.label_name..'\n')
end
function dst.jnz(stmt)
  io.write('  if '..stmt.register..' goto '..stmt.label_name..'\n')
end
function dst.jeq(stmt)
  io.write('  if '..stmt.register_x..' == '..stmt.register_y..' goto '..stmt.label_name..'\n')
end
function dst.jneq(stmt)
  io.write('  if '..stmt.register_x..' != '..stmt.register_y..' goto '..stmt.label_name..'\n')
end
function dst.jlt(stmt)
  io.write('  if '..stmt.register_x..' < '..stmt.register_y..' goto '..stmt.label_name..'\n')
end
function dst.jgt(stmt)
  io.write('  if '..stmt.register_x..' > '..stmt.register_y..' goto '..stmt.label_name..'\n')
end
function dst.jleq(stmt)
  io.write('  if '..stmt.register_x..' <= '..stmt.register_y..' goto '..stmt.label_name..'\n')
end
function dst.jgeq(stmt)
  io.write('  if '..stmt.register_x..' >= '..stmt.register_y..' goto '..stmt.label_name..'\n')
end

local function dump_statement(stmt)
  local h = dst[stmt.type]
  if h then
    h(stmt)
  else
    error('Unknown IR statement type `'..stmt.type..'`')
  end
end

local function dump_subroutine(subr)
  io.write('  ; temp_count: '..subr.meta.temp_count..'\n')
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
  io.write('\n')
end

function ir.dump(ir)
  for i,subr in ipairs(ir.subroutines) do
    dump_subroutine(subr)
  end
end

return ir

