
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
-- Statement construction

-- Unless otherwise specified, table arguments are copied

local ir = {}

function ir.literal(num)
  assert(type(num) == 'number')
  return num
end

function ir.tempreg(index)
  assert(type(index) == 'number')
  return 'r'..index
end

function ir.argreg(index)
  assert(type(index) == 'number')
  return 'a'..index
end

function ir.retreg(index)
  assert(type(index) == 'number')
  return 'b'..index
end

function ir.istempreg(reg)
  assert(type(reg) == 'string')
  return reg:sub(1,1) == 'r'
end

function ir.isargreg(reg)
  assert(type(reg) == 'string')
  return reg:sub(1,1) == 'a'
end

function ir.isretreg(reg)
  assert(type(reg) == 'string')
  return reg:sub(1,1) == 'b'
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
-- ir.subroutine - Subroutine definition and builder

local subroutine_methods = { }
local subroutine_meta = { __index = subroutine_methods }

function subroutine_methods.alloc_temporary(self, count)
  assert(type(count) == 'number')
  -- Simply return the next n temporaries
  local first = self.size_registers
  local alloc = { type = 'temporary', first = first, count = count }
  -- Track high-water mark
  self.size_registers = first + count
  return alloc
end

function subroutine_methods.alloc_stack(self, offset, size)
  assert(type(offset) == 'number')
  assert(type(size) == 'number')
  -- Return the allocation exactly as specified, so that the compiler may re-use old stack space
  local alloc = { type = 'stack', id = #self.locals + 1, offset = offset, size = size }
  -- Remember this block for use in statements
  self.locals[#self.locals+1] = alloc
  -- Track stack's high-water mark
  if offset + size > self.size_stack then
    self.size_stack = offset + size
  end
  return alloc
end

function subroutine_methods.alloc_argument(self, count)
  assert(type(count) == 'number')
  -- Simply return the next n argument registers
  local first = self.size_arguments
  local alloc = { type = 'argument', first = first, count = count }
  -- Track high-water mark
  self.size_arguments = first + count
  return alloc
end

function subroutine_methods.alloc_return(self, count)
  assert(type(count) == 'number')
  -- Simply return the next n return registers
  local first = self.size_returns
  local alloc = { type = 'return', first = first, count = count }
  -- Track high-water mark
  self.size_returns = first + count
  return alloc
end

function subroutine_methods.add_statement(self, stmt)
  assert(type(stmt) == 'table')
  self.statements[#self.statements+1] = stmt
  return stmt
end

local function subroutine_new(name)
  assert(type(name) == 'string')
  local subr = { name = name,
                 locals = { },
                 size_arguments = 0,
                 size_returns = 0,
                 size_stack = 0,
                 size_registers = 0,
                 statements = { } }
  return setmetatable(subr, subroutine_meta)
end

ir.subroutine = subroutine_new

----------------------------------------------------------------------------------------------------
-- ir.program - Program definition and builder

local program_methods = { }
local program_meta = { __index = program_methods }

function program_methods.add_subroutine(self, name)
  local ir_subr = subroutine_new(name)
  table.insert(self.subroutines, ir_subr)
  return ir_subr
end

local function program_new()
  local prog = { subroutines = { } }
  return setmetatable(prog, program_meta)
end

ir.program = program_new

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
  io.write('sub '..subr.name..' : '..subr.size_arguments..' -> '..subr.size_returns..'\n')
  for i,stmt in ipairs(subr.statements) do
    dump_statement(stmt)
  end
end

-- Pretty-prints the given ir.program object to io.output
function ir.dump(ir_prog)
  for i,subr in ipairs(ir_prog.subroutines) do
    ir.dump_subroutine(subr)
  end
end

ir.statement_string = stmt_string

return ir

