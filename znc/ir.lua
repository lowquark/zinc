
----------------------------------------------------------------------------------------------------
-- IR - (I)ntermediate (R)epresentation
----------------------------------------------------------------------------------------------------

-- TODO: w, x, y or a list of call inputs is easy to unify into 0, 1, 2 ... etc

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
-- Statement construction
--
-- Unless otherwise specified, table arguments are copied

local ir = {}

-- Returns an identifier for a literal number
function ir.literal(num)
  assert(type(num) == 'number')
  return tostring(num)
end

-- Returns an identifier for the stack (object) at the given index
function ir.localid(index)
  assert(type(index) == 'number')
  return 's'..tostring(index)
end

-- Returns an identifier for the register at the given index
function ir.registerid(index)
  assert(type(index) == 'number')
  return 'r'..index
end

-- Returns an identifier for the argument register at the given index
function ir.argumentid(index)
  assert(type(index) == 'number')
  return 'a'..index
end

-- Returns an identifier for the return register at the given index
function ir.returnid(index)
  assert(type(index) == 'number')
  return 'b'..index
end

function ir.mov(reg_z, reg_x)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  return { type = 'mov',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.neg(reg_z, reg_x)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  return { type = 'neg',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.bnot(reg_z, reg_x)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  return { type = 'bnot',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.lnot(reg_z, reg_x)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  return { type = 'lnot',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.add(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'add',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.sub(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'sub',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.mul(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'mul',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.div(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'div',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.eq(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'eq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.neq(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'neq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.lt(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'lt',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.gt(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'gt',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.leq(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'leq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.geq(reg_z, reg_x, reg_y)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'geq',
           register_z = reg_z,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.ldr(reg_z, reg_x)
  assert(type(reg_z) == 'string')
  assert(type(reg_x) == 'string')
  return { type = 'ldr',
           register_z = reg_z,
           register_x = reg_x }
end

function ir.str(reg_w, reg_x)
  assert(type(reg_w) == 'string')
  assert(type(reg_x) == 'string')
  return { type = 'str',
           register_w = reg_w,
           register_x = reg_x }
end

function ir.call(return_regs, name, argument_regs)
  assert(type(return_regs) == 'table')
  assert(type(name) == 'string')
  assert(type(argument_regs) == 'table')
  return { type = 'call',
           return_regs = deepcopy(return_regs),
           name = name,
           argument_regs = deepcopy(argument_regs) }
end

function ir.ret()
  return { type = 'ret' }
end

-- Defines a jump label
function ir.label(name)
  assert(type(name) == 'string')
  return { type = 'label', name = name }
end

-- Unconditional jump
function ir.jmp(label_name)
  assert(type(label_name) == 'string')
  return { type = 'jmp', label_name = label_name }
end

-- Jump if equal
function ir.jeq(label_name, reg_x, reg_y)
  assert(type(label_name) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'jeq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if not equal
function ir.jneq(label_name, reg_x, reg_y)
  assert(type(label_name) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'jneq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if zero
function ir.jz(label_name, reg)
  assert(type(label_name) == 'string')
  assert(type(reg) == 'string')
  return { type = 'jz',
           label_name = label_name,
           register_x = reg }
end

-- Jump if nonzero
function ir.jnz(label_name, reg)
  assert(type(label_name) == 'string')
  assert(type(reg) == 'string')
  return { type = 'jnz',
           label_name = label_name,
           register_x = reg }
end

-- Jump if less than
function ir.jlt(label_name, reg_x, reg_y)
  assert(type(label_name) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'jlt',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if greater than
function ir.jgt(label_name, reg_x, reg_y)
  assert(type(label_name) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'jgt',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if less than or equal
function ir.jleq(label_name, reg_x, reg_y)
  assert(type(label_name) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'jleq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

-- Jump if greater than or equal
function ir.jgeq(label_name, reg_x, reg_y)
  assert(type(label_name) == 'string')
  assert(type(reg_x) == 'string')
  assert(type(reg_y) == 'string')
  return { type = 'jgeq',
           label_name = label_name,
           register_x = reg_x,
           register_y = reg_y }
end

function ir.stackaddr(reg_z, index)
  assert(type(reg_z) == 'string')
  assert(type(index) == 'number')
  return { type = 'stackaddr',
           register_z = reg_z,
           index = index }
end

----------------------------------------------------------------------------------------------------
-- ir.subroutine - Subroutine definition

local subroutine_methods = { }
local subroutine_meta = { __index = subroutine_methods }

function ir.is_subroutine(subr)
  return getmetatable(subr) == subroutine_meta
end

function ir.subroutine(name)
  assert(type(name) == 'string')
  local subr = { name = name,
                 locals = { },
                 size_stack = 0,
                 size_registers = 0,
                 size_arguments = 0,
                 size_returns = 0,
                 statements = { } }
  return setmetatable(subr, subroutine_meta)
end

----------------------------------------------------------------------------------------------------
-- ir.program - IR program definition

local program_methods = { }
local program_meta = { __index = program_methods }

function ir.program(main_module)
  local prog = { subroutines = { }, main_module = main_module }
  return setmetatable(prog, program_meta)
end

----------------------------------------------------------------------------------------------------
-- Construction helpers

-- Creates a new local variable in the given ir subroutine
--   subr   : ir.subroutine
--   offset : integer stack offset
--   size   : integer variable size
--   -> IR identifier string
function ir.create_local(subr, offset, size)
  local next_idx = #subr.locals
  subr.locals[next_idx+1] = { offset = offset, size = size }
  -- Track high water mark
  if offset + size > subr.size_stack then
    subr.size_stack = offset + size
  end
  -- Return the IR identifier for this local
  return ir.localid(next_idx)
end

-- Creates a new register in the given ir subroutine
--   subr : ir.subroutine
--   -> IR identifier string
function ir.create_register(subr)
  local next_idx = subr.size_registers
  subr.size_registers = next_idx + 1
  return ir.registerid(next_idx)
end

-- Creates a new argument register in the given ir subroutine
--   subr : ir.subroutine
--   -> IR identifier string
function ir.create_argument(subr)
  local next_idx = subr.size_arguments
  subr.size_arguments = next_idx + 1
  return ir.argumentid(next_idx)
end

-- Creates a new return register in the given ir subroutine
--   subr : ir.subroutine
--   -> IR identifier string
function ir.create_return(subr)
  local next_idx = subr.size_returns
  subr.size_returns = next_idx + 1
  return ir.returnid(next_idx)
end

-- Appends an instruction to the statements of the given subroutine
--   subr : ir.subroutine
--   stmt : ir.statement
function ir.add_statement(subr, stmt)
  local stmts = subr.statements
  stmts[#stmts + 1] = stmt
end

-- Appends a subroutine to the given program
--   prog : ir.program
--   subr : ir.subroutine
function ir.add_subroutine(prog, subr)
  table.insert(prog.subroutines, subr)
  prog.subroutines[subr.name] = subr
  return subr
end

function ir.find_subroutine(prog, name)
  return prog.subroutines[name]
end

----------------------------------------------------------------------------------------------------
-- Pretty printing - Statement to string conversion

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
    for i,return_reg in ipairs(stmt.return_regs) do
      if i == 1 then
        str = str..return_reg
      else
        str = str..', '..return_reg
      end
    end
    str = str..' := '
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

function sst.stackaddr(stmt)
  return stmt.register_z..' := &(s'..stmt.index..')'
end

function sst.ldr(stmt)
  return stmt.register_z..' := *('..stmt.register_x..')'
end

function sst.str(stmt)
  return '*('..stmt.register_w..') := '..stmt.register_x
end

local function stmt_string(stmt)
  local h = sst[stmt.type]
  if h then
    return h(stmt)
  else
    error('Unknown IR statement type `'..tostring(stmt.type)..'`')
  end
end

----------------------------------------------------------------------------------------------------
-- Pretty printing - Dumping to io.output

local function dump_statement(stmt)
  if stmt.type == 'label' then
    io.write(stmt_string(stmt)..'\n')
  else
    io.write('  '..stmt_string(stmt)..'\n')
  end
end

function ir.dump_subroutine(subr)
  io.write('sub '..subr.name..' ('..subr.size_arguments..') -> ('..subr.size_returns..')\n')
  for i,stmt in ipairs(subr.statements) do
    dump_statement(stmt)
  end
end

-- Pretty-prints the given program object to io.output
function ir.dump(ir_prog)
  for i,subr in ipairs(ir_prog.subroutines) do
    ir.dump_subroutine(subr)
  end
end

-- Validates the given program
-- Well, it will
function ir.validate(ir_prog)
  -- TODO: Check for unknown locals
  -- TODO: Check for invalid registers
  -- TODO: Check for unknown labels
  -- TODO: Check for unknown functions
  -- TODO: Check for literal destination operands
end

ir.statement_string = stmt_string

return ir

