
----------------------------------------------------------------------------------------------------
-- Generate x86-64 code from IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local lsra = require 'lsra'

local function list_contains(t, v)
  for i=1,#t do
    if t[i] == v then return true end
  end
  return false
end

-- These registers are used for memory reads and backing up values
--   %rax
--   %rcx

-- These registers are used to store IR temporaries
--   %rbx
--   %rdx
--   %r8
--   %r9
--   %r10
--   %r11

-- (o)perand (t)emporary (r)egister (t)able
local otrt = {
  '%rbx',
  '%rdx',
  '%r8',
  '%r9',
  '%r10',
  '%r11',
}

-- (r)egister (b)yte (f)orm (t)able
local rbft = {
  ['%rbx'] = '%bl',
  ['%rdx'] = '%dl',
  ['%r8'] = '%r8b',
  ['%r9'] = '%r9b',
  ['%r10'] = '%r10b',
  ['%r11'] = '%r11b',
}

-- (i)nverse (s)et (c)omparison (i)nstruction (t)able
local iscit = {
  ['sete'] = 'setne',
  ['setne'] = 'sete',
  ['setl'] = 'setge',
  ['setge'] = 'setl',
  ['setg'] = 'setle',
  ['setle'] = 'setg',
}

-- (e)mit (s)tatement (t)able
local est = {}

----------------------------------------------------------------------------------------------------
-- Utilities

-- Computes the x86 instruction operand corresponding to the given IR register/literal
-- Returns the x86 operand and its corresponding type
local function operand(ctx, ir_name)
  local typestr = string.sub(ir_name, 1, 1)
  if typestr == 'r' then
    -- Temporary access
    local index = tonumber(string.match(ir_name, 'r(%d+)'))
    if not index then
      error('Invalid IR temporary register `'..ir_name..'`')
    end
    local op = otrt[index+1]
    if op then
      return op, 'register'
    else
      error('No hardware register available for '..ir_name)
    end
  elseif typestr == 's' then
    -- Stack access
    local index = tonumber(string.match(ir_name, 's(%d+)'))
    if not index then
      error('Invalid IR stack register `'..ir_name..'`')
    end
    local stack_offset = -8*(index + 1)
    return stack_offset..'(%rbp)', 'memory'
  elseif typestr == 'i' then
    -- Call input access
    local index = tonumber(string.match(ir_name, 'i(%d+)'))
    if not index then
      error('Invalid IR input/output register `'..ir_name..'`')
    end
    local stack_offset = 8*(ctx.subroutine.input_size - index + 1)
    return stack_offset..'(%rbp)', 'memory'
  else
    local integer = tonumber(ir_name)
    if integer then
      return '$'..integer, 'literal'
    else
      error('Failed to generate operand for `'..ir_name..'`')
    end
  end
end

-- Computes the least-significant-byte form of the given x86 operand
local function reg_byte_form(op, type)
  if type == 'register' then
    local r = rbft[op]
    if not r then
      error('Register byte form for `'..op..'` not yet set')
    end
    return r
  end
  -- hmmm, since we're LSB first this should still work for memory addresses
  return op
end

----------------------------------------------------------------------------------------------------
-- Code generation

local function convert_label(ctx, ir_name)
  return '_s'..ctx.subroutine_id..'_'..ir_name
end

local function emit(ctx, instr)
  ctx.outfile:write('\t'..instr..'\n')
end

local function emit_noindent(ctx, instr)
  ctx.outfile:write(instr..'\n')
end

local function emit_comment(ctx, str)
  ctx.outfile:write('\t#  '..str..'\n')
end

local function emit_label(ctx, label)
  ctx.outfile:write(label..':\n')
end

local function emit_line(ctx)
  ctx.outfile:write('\n')
end

local function emit_prologue(ctx)
  emit(ctx, 'push %rbp')
  emit(ctx, 'mov  %rsp, %rbp')
end

local function emit_epilogue(ctx)
  emit(ctx, 'mov  %rbp, %rsp')
  emit(ctx, 'pop  %rbp')
  emit(ctx, 'ret')
end

local function emit_stack_alloc(ctx, size)
  -- Allocate by subtracting from %rsp
  emit(ctx, 'sub  $'..(size*8)..', %rsp')
  -- Track stack index (relative to %rbp)
  ctx.stack_index = ctx.stack_index + size
end

local function emit_stack_dealloc(ctx, size)
  -- Deallocate by adding to %rsp
  emit(ctx, 'add  $'..(size*8)..', %rsp')
  -- Track stack index (relative to %rbp)
  ctx.stack_index = ctx.stack_index - size
end

local function emit_push(ctx, op_x, type_x)
  emit(ctx, 'push '..op_x)
  ctx.stack_index = ctx.stack_index + 1
end

local function emit_pop(ctx, op_x, type_x)
  emit(ctx, 'pop '..op_x)
  ctx.stack_index = ctx.stack_index - 1
end

local function emit_mov(ctx, op_x, type_x, op_z, type_z)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_z == 'literal' then error('Invalid instruction') end
  -- Optimize out useless moves
  if op_x == op_z then return end
  -- Move directly if possible
  if type_x == 'register' or type_z == 'register' then
    -- If either is a register, only one memory reference will be possible
    emit(ctx, 'mov  '..op_x..', '..op_z)
  else
    -- Into the accumulator it goes
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'mov  %rax, '..op_z)
  end
end

local function emit_binary_arith_op(ctx, instr, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_z == 'literal' then error('Invalid instruction') end
  if type_z == 'register' then
    -- op_z is a register, so only a single memory reference will be possible here
    if op_x ~= op_z then
      emit(ctx, 'mov  '..op_x..', '..op_z)
    end
    emit(ctx, instr..' '..op_y..', '..op_z)
  else
    -- Into the accumulator it goes
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, instr..' '..op_y..', %rax')
    emit(ctx, 'mov  %rax, '..op_z)
  end
end

local function emit_comparison(ctx, set_instr, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_z == 'literal' then error('Invalid instruction') end

  if not iscit[set_instr] then
    error('Invalid set comparison instruction `'..set_instr..'`')
  end

  if type_y == 'literal' then
    -- cmp requires its second argument not be a literal, so swap everything
    op_x, op_y = op_y, op_x
    type_x, type_y = type_y, type_x
    io.write(set_instr..' -> '..iscit[set_instr]..'\n')
    --set_instr = iscit[set_instr]
  end

  if type_z == 'register' then
    -- op_z is a register, so only a single memory reference will be possible here
    if op_x ~= op_z then
      emit(ctx, 'mov  '..op_x..', '..op_z)
    end
    emit(ctx, 'cmp  '..op_z..', '..op_y)
    emit(ctx, 'mov  $0, '..op_z)
    emit(ctx, set_instr..' '..reg_byte_form(op_z, type_z))
  else
    -- Into the accumulator it goes
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'cmp  %rax, '..op_y)
    emit(ctx, 'mov  $0, %rax')
    emit(ctx, set_instr..' %al')
    emit(ctx, 'mov  %rax, '..op_z)
  end
end

local function emit_jump_comparison(ctx, jump_instr, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_x == 'literal' and type_y == 'literal' then error('Invalid instruction') end
  if type_x == 'register' or type_z == 'register' then
    -- Either is a register, so only a single memory reference will be possible here
    emit(ctx, 'cmp  '..op_x..', '..op_y)
  else
    -- Into the accumulator it goes
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'cmp  %rax, '..op_y)
  end
  -- Jump based on condition
  emit(ctx, jump_instr..' '..convert_label(ctx, ir_stmt.label_name))
end

function emit_statement(ctx, ir_stmt)
  local h = est[ir_stmt.type]
  if h then
    h(ctx, ir_stmt)
  else
    error('Unknown IR statement type `'..ir_stmt.type..'`')
  end
end

function est.mov(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
end

function est.neg(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
  emit(ctx, 'negq '..op_z)
end

function est.bnot(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
  emit(ctx, 'notq '..op_z)
end

function est.lnot(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
  emit(ctx, 'cmp  $0, '..op_z)
  emit(ctx, 'mov  $0, '..op_z)
  emit(ctx, 'sete '..reg_byte_form(op_z, type_z))
end

function est.add(ctx, ir_stmt)
  emit_binary_arith_op(ctx, 'addq', ir_stmt)
end

function est.sub(ctx, ir_stmt)
  emit_binary_arith_op(ctx, 'subq', ir_stmt)
end

function est.mul(ctx, ir_stmt)
  emit_binary_arith_op(ctx, 'imul', ir_stmt)
end

function est.div(ctx, ir_stmt)
  -- Though, it's pretty marginal compared to how many cycles integer division itself takes.
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  if type_z == 'literal' then error('Invalid instruction') end
  -- Backup %rdx if applicable
  if op_z ~= '%rdx' then
    emit(ctx, 'mov  %rdx, %rcx')
  end
  -- Place our dividend in %rax
  emit(ctx, 'mov  '..op_x..', %rax')
  -- Sign extend %rax into %rdx.
  -- This clobbers %rdx, and kills the crab.
  emit(ctx, 'cqto')
  -- Divide [%rdx:%rax] by the divisor (op_y)
  emit(ctx, 'idivq '..op_y)
  -- Move the quotient into our destination (op_z)
  emit(ctx, 'mov  %rax, '..op_z)
  -- Restore %rdx if applicable
  if op_z ~= '%rdx' then
    emit(ctx, 'mov  %rcx, %rdx')
  end
end

function est.eq(ctx, ir_stmt)
  emit_comparison(ctx, 'sete', ir_stmt)
end

function est.neq(ctx, ir_stmt)
  emit_comparison(ctx, 'setne', ir_stmt)
end

function est.lt(ctx, ir_stmt)
  emit_comparison(ctx, 'setl', ir_stmt)
end

function est.gt(ctx, ir_stmt)
  emit_comparison(ctx, 'setg', ir_stmt)
end

function est.leq(ctx, ir_stmt)
  emit_comparison(ctx, 'setle', ir_stmt)
end

function est.geq(ctx, ir_stmt)
  emit_comparison(ctx, 'setge', ir_stmt)
end

function est.call(ctx, ir_stmt)
  local return_ops = { }
  local return_types = { }
  local argument_ops = { }
  local argument_types = { }
  for i,reg in ipairs(ir_stmt.return_regs) do
    if reg == '~' then
      return_ops[i], return_types[i] = nil, nil
    else
      return_ops[i], return_types[i] = operand(ctx, reg)
    end
  end
  for i,reg in ipairs(ir_stmt.argument_regs) do
    argument_ops[i], argument_types[i] = operand(ctx, reg)
  end
  -- Save all possibly clobbered registers
  -- TODO: Only save live registers!
  local saved_regs = { }
  for i=1,#otrt do
    local reg = otrt[i]
    if not list_contains(return_ops, reg) then
      emit_push(ctx, reg)
      saved_regs[#saved_regs+1] = reg
    end
  end
  local arg_stack_idx = ctx.stack_index
  local input_size = math.max(#ir_stmt.return_regs, #ir_stmt.argument_regs)
  -- Allocate stack argument space
  emit_stack_alloc(ctx, input_size)
  -- Push arguments
  for i=1,#argument_ops do
    emit_mov(ctx, argument_ops[i], argument_types[i], (-8*(arg_stack_idx+i))..'(%rbp)', 'memory')
  end
  -- Emit actual call
  emit(ctx, 'call '..ir_stmt.name)
  -- Grab return values
  for i=1,#ir_stmt.return_regs do
    if return_ops[i] then
      emit_mov(ctx, (-8*(arg_stack_idx+i))..'(%rbp)', 'memory', return_ops[i], return_types[i])
    end
  end
  -- Deallocate stack argument space
  emit_stack_dealloc(ctx, input_size)
  -- Restore all previously pushed registers
  for i=#saved_regs,1,-1 do
    emit(ctx, 'pop  '..saved_regs[i])
    ctx.stack_index = ctx.stack_index - 1
  end
end

function est.ret(ctx, ir_stmt)
  -- Any return values are assumed to have been already set
  emit_epilogue(ctx)
end

function est.label(ctx, ir_stmt)
  emit_label(ctx, convert_label(ctx, ir_stmt.name))
end

function est.jmp(ctx, ir_stmt)
  emit(ctx, 'jmp '..convert_label(ctx, ir_stmt.label_name))
end

function est.jz(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  if type_x == 'literal' then
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'cmp  $0, %rax')
  else
    emit(ctx, 'cmp  $0, '..op_x)
  end
  emit(ctx, 'je '..convert_label(ctx, ir_stmt.label_name))
end

function est.jnz(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  if type_x == 'literal' then
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'cmp  $0, %rax')
  else
    emit(ctx, 'cmp  $0, '..op_x)
  end
  emit(ctx, 'jne '..convert_label(ctx, ir_stmt.label_name))
end

function est.jeq(ctx, ir_stmt)
  emit_jump_comparison(ctx, 'je', ir_stmt)
end

function est.jneq(ctx, ir_stmt)
  emit_jump_comparison(ctx, 'jne', ir_stmt)
end

function est.jlt(ctx, ir_stmt)
  emit_jump_comparison(ctx, 'jl', ir_stmt)
end

function est.jgt(ctx, ir_stmt)
  emit_jump_comparison(ctx, 'jg', ir_stmt)
end

function est.jleq(ctx, ir_stmt)
  emit_jump_comparison(ctx, 'jle', ir_stmt)
end

function est.jgeq(ctx, ir_stmt)
  emit_jump_comparison(ctx, 'jge', ir_stmt)
end

local function emit_subroutine(ctx, ir_subr)
  -- Initialize context
  ctx.stack_index = 0
  ctx.subroutine = ir_subr
  -- Emit header
  emit(ctx, '.globl '..ir_subr.name)
  emit(ctx, '.type '..ir_subr.name..', @function')
  emit_label(ctx, ir_subr.name)
  emit_prologue(ctx)
  -- Reserve stack space for this function (part of the prologue, really)
  if ir_subr.stack_size then
    emit_stack_alloc(ctx, ir_subr.stack_size)
  end
  -- Emit statements
  for i,ir_stmt in ipairs(ir_subr.statements) do
    emit_comment(ctx, ir.statement_string(ir_stmt))
    emit_statement(ctx, ir_stmt)
  end
  -- Emit default return statement if the last one wasn't a return
  local n = #ir_subr.statements
  if n == 0 or ir_subr.statements[n].type ~= 'ret' then
    emit_epilogue(ctx)
  end
end

-- Translates the given IR program into x86-64 AT&T assembler, and writes to outfile
-- XXX: This modifies ir_prog!
local function generate(ir_prog, outfile)
  local ctx = { }
  ctx.outfile = outfile

  emit(ctx, '.text')
  emit_line(ctx)

  for i,ir_subr in ipairs(ir_prog.subroutines) do
    -- Allocate hardware registers
    lsra(ir_subr, #otrt)
    -- Print to console for shits and giggles
    ir.dump_subroutine(ir_subr)
    -- Generate code
    ctx.subroutine_id = i
    emit_subroutine(ctx, ir_subr)
    emit_line(ctx)
  end
  -- A nice, hardcoded main function
  emit(ctx, '.globl main')
  emit(ctx, '.type main, @function')
  emit_label(ctx, 'main')
  emit(ctx, 'push %rbp')
  emit(ctx, 'mov  %rsp, %rbp')
  emit(ctx, 'sub  $8, %rsp')
  -- TODO: The module containing main needs to be specified as a compiler argument
  emit(ctx, 'call my_module$main')
  emit(ctx, 'mov  -8(%rbp), %rax')
  emit(ctx, 'add  $8, %rsp')
  emit(ctx, 'mov  %rbp, %rsp')
  emit(ctx, 'pop  %rbp')
  emit(ctx, 'ret')
  emit_line(ctx)
end

return generate

