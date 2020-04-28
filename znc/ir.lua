
----------------------------------------------------------------------------------------------------
-- IR - (I)ntermediate (R)epresentation
----------------------------------------------------------------------------------------------------

local pprint = require 'pprint'

--[[

mov
neg
bnot
lnot
add
sub
mul
div
eq
neq
lt
gt
leq
geq
salloc
sfree
begincall
endcall
call
ret
label
jmp
jeq
jneq
jz
jnz
jlt
jgt
jleq
jgeq
subroutine

]]

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
-- Tree construction

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
           register = reg }
end
-- Jump if nonzero
function ir.jnz(label_name, reg)
  assert(label_name)
  assert(reg)
  return { type = 'jnz',
           label_name = label_name,
           register = reg }
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

function ir.subroutine(args, rets, stmts, meta)
  assert(args)
  assert(rets)
  assert(stmts)
  return { type = 'subroutine',
           arguments = args,
           returns = rets,
           statements = stmts,
           meta = meta or { } }
end

----------------------------------------------------------------------------------------------------
-- Pretty printing

-- (s)atement (s)tring (t)able
local sst = {}

function sst.salloc(stmt)
  return 'salloc '..stmt.size
end
function sst.sfree(stmt)
  return 'sfree '..stmt.size
end

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

function sst.begincall(stmt)
  return 'begincall '..stmt.size
end
function sst.endcall(stmt)
  return 'endcall'
end
function sst.call(stmt)
  return 'call '..stmt.name
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
  return 'if !'..stmt.register..' goto '..stmt.label_name
end
function sst.jnz(stmt)
  return 'if '..stmt.register..' goto '..stmt.label_name
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
    error('Unknown IR statement type `'..stmt.type..'`')
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

-- Pretty-prints the given IR program object to io.output
function ir.dump(ir_prog)
  for i,subr in ipairs(ir_prog.subroutines) do
    ir.dump_subroutine(subr)
  end
end

----------------------------------------------------------------------------------------------------
-- Opcode information

local function inputs(stmt)
  -- Quadruplet representation makes this convenient
  return stmt.register_x, stmt.register_y
end

local function outputs(stmt)
  -- Quadruplet representation makes this convenient
  return stmt.register_z
end

----------------------------------------------------------------------------------------------------
-- Other utilities

function ir.dup(ir_any)
  -- No metatables or recursion to worry about here -we can easily duplicate any portion of any IR
  -- object.
  return deepcopy(ir_any)
end

-- Computes the maximum lifetime interval for non-input and non-argument registers
-- Returns each an array of register names and their intervals, in order of first start index
local function compute_max_lifetimes(subr)
  local end_idx = { }
  local seen = { }
  local lifetimes = { }
  local n = #subr.statements
  -- Search backward to find last reference to each register
  for k=n,1,-1 do
    local ir_stmt = subr.statements[k]
    local reg_x, reg_y = inputs(ir_stmt)
    if reg_x and not end_idx[reg_x] then
      local c = reg_x:sub(1,1)
      if c ~= 'a' and c ~= 'i' then
        end_idx[reg_x] = k
      end
    end
    if reg_y and not end_idx[reg_y] then
      local c = reg_y:sub(1,1)
      if c ~= 'a' and c ~= 'i' then
        end_idx[reg_y] = k
      end
    end
  end
  -- Search forward to find first definition of each register, and when found, attempt to create an
  -- interval with an associated end index
  for k=1,n do
    local ir_stmt = subr.statements[k]
    local reg_z = outputs(ir_stmt)
    if reg_z and not seen[reg_z] then
      -- Mark visited
      seen[reg_z] = true
      local c = reg_z:sub(1,1)
      if c ~= 'a' and c ~= 'i' then
        -- Potential first definition
        local k_begin = k
        local k_end = end_idx[reg_z]
        if k_end and k_end > k_begin then
          -- We also happen to be iterating in order of start index, so add it to the result
          table.insert(lifetimes, { reg = reg_z, k_begin = k_begin, k_end = k_end })
        end
      end
    end
  end
  return lifetimes
end

-- Quick imlpementation of the linear scan algorithm
-- web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
-- Converts the given subroutine into one which relies on only the given temporary registers
function ir.allocate_registers_lsra(subr, num_target_regs)
  -- Compute lifetimes of each register
  local lifetimes = compute_max_lifetimes(subr)
  for k,interval in ipairs(lifetimes) do
    io.write(interval.reg..' is live on ['..interval.k_begin..', '..interval.k_end..')\n')
  end
  -- List of live variables/intervals, in order of increasing k_end
  local active = { }
  -- Generate list of free "hardware" registers
  local free_regs = { }
  for k=1,num_target_regs do
    free_regs[k] = 'r'..(k-1)
  end
  local new_mapping = { }
  -- Erase active intervals which have ended
  local function expire_old(k)
    local n = #active
    -- Partially remove expired ranges
    for j=1,n do
      local interval_j = active[j]
      if interval_j.k_end >= k then
        -- This interval is still going, so the rest are too. Exit early
        break
      else
        active[j] = nil
        -- Free now
        table.insert(free_regs, interval_j.target_reg)
      end
    end
    -- Purge nil values
    local j = 1
    for i=1,n do
      local value = active[i]
      if value then
        if i ~= j then
          active[j] = active[i]
          active[i] = nil
        end
        j = j + 1
      end
    end
  end
  -- Inserts an interval into active set, maintaining order
  local function add_active(interval)
    local n = #active
    if n == 0 then
      active[1] = interval
    else
      for j=1,n do
        local interval_j = active[j]
        if interval.k_end <= interval_j.k_end then
          local tmp
          for k=j,n do
            tmp = active[k]
            active[k] = interval
            interval = tmp
          end
          break
        end
      end
      active[n+1] = interval
    end
  end
  -- Main loop
  for k,interval_i in ipairs(lifetimes) do
    expire_old(interval_i.k_begin)
    if #active == num_target_regs then
      pprint(active)
      pprint(free_regs)
      error('TODO: Implement spilling\n')
    else
      local ir_reg = interval_i.reg
      local target_reg = table.remove(free_regs, 1)
      io.write(ir_reg..' <- '..target_reg..'\n')
      new_mapping[ir_reg] = target_reg
      interval_i.target_reg = target_reg
      add_active(interval_i)
    end
  end
  -- Reindex everything according to remapped registers
  for k,ir_stmt in ipairs(subr.statements) do
    local new_reg
    if ir_stmt.register_x then
      new_reg = new_mapping[ir_stmt.register_x]
      if new_reg then ir_stmt.register_x = new_reg end
    end
    if ir_stmt.register_y then
      new_reg = new_mapping[ir_stmt.register_y]
      if new_reg then ir_stmt.register_y = new_reg end
    end
    if ir_stmt.register_z then
      new_reg = new_mapping[ir_stmt.register_z]
      if new_reg then ir_stmt.register_z = new_reg end
    end
  end
end

ir.statement_string = stmt_string

return ir

