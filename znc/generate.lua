
local ir = require 'ir'

--------------------------------------------------------------------------------
-- Generate x86-64 code from IR
--------------------------------------------------------------------------------

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
  -- Note: We don't track push/pop!
  ctx.stack_index = ctx.stack_index + size
end

local function emit_stack_dealloc(ctx, size)
  -- Deallocate by adding to %rsp
  emit(ctx, 'add  $'..(size*8)..', %rsp')
  -- Track stack index (relative to %rbp)
  ctx.stack_index = ctx.stack_index - size
end

-- (o)perand (t)emporary (r)egister (t)able
local otrt = {
  '%rax',
  '%rcx',
  '%rdx',
  '%r8',
  '%r9',
  '%r10',
  '%r11',
}

local function operand(ctx, name)
  local typestr = string.sub(name, 1, 1)
  if typestr == 'r' then
    -- Temporary access
    local index = string.match(name, 'r(%d+)')
    if not index then
      error('Invalid IR temporary register `'..name..'`')
    end
    index = tonumber(index)
    if index > ctx.temp_count then
      error('Invalid IR temp register `'..name..'` exceeds maximum of `'..ctx.temp_count..'`')
    end
    local op = otrt[index+1]
    if op then
      return op, 'register'
    else
      error('TODO: Implement spilling')
      --return tostring(-8*(index-1))..'(%rbp)', 'memory'
    end
  elseif typestr == 's' then
    -- Stack access
    local index = string.match(name, 's(%d+)')
    if index then
      index = tonumber(index)
    else
      error('Invalid IR stack register `'..name..'`')
    end
    local stack_offset = -8*(index + 1)
    return stack_offset..'(%rbp)', 'memory'
  elseif typestr == 'a' then
    -- Call argument access
    local call_data = ctx.call_data_stack[#ctx.call_data_stack]
    if not call_data then
      error('No matching begincall instruction for argument register `'..name..'`')
    end
    local index = string.match(name, 'a(%d+)')
    if index then
      index = tonumber(index)
    else
      error('Invalid IR argument register `'..name..'`')
    end
    -- Arguments are always relative to 
    local stack_offset = -8*(call_data.args_index + index + 1)
    return stack_offset..'(%rbp)', 'memory'
  elseif typestr == 'i' then
    -- Call input access
    local index = string.match(name, 'i(%d+)')
    if index then
      index = tonumber(index)
    else
      error('Invalid IR input/output register `'..name..'`')
    end
    local stack_offset = 8*(ctx.input_size - index + 1)
    return stack_offset..'(%rbp)', 'memory'
  else
    local integer = tonumber(name)
    if not integer then
      error('Failed to generate operand for `'..name..'`')
    end
    return '$'..integer, 'literal'
  end
end

-- These registers must be preserved:
--   %rbx
--   %rbp
--   %r12
--   %r13
--   %r14
--   %r15
-- These registers are fair game
local rbft = {
  ['%rax'] = '%al',
  ['%rcx'] = '%cl',
  ['%rdx'] = '%dl',
  ['%r8'] = '%r8b',
  ['%r9'] = '%r9b',
  ['%r10'] = '%r10b',
  ['%r11'] = '%r11b',
}

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

local function emit_mov(ctx, op_x, type_x, op_z, type_z)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_z == 'literal' then error('Invalid instruction') end
  -- Optimize out useless moves
  if op_x == op_z then return end
  -- Move if possible
  if type_x == 'register' or type_z == 'register' then
    -- If either is a register, only one memory reference is possible
    emit(ctx, 'mov  '..op_x..', '..op_z)
  else
    emit(ctx, 'push %rax')
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'mov  %rax, '..op_z)
    emit(ctx, 'pop  %rax')
  end
end

local function emit_binary_arith_op(ctx, instr, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_z == 'literal' then error('Invalid instruction') end
  if type_z == 'register' then
    -- op_z is a register, so only a single memory reference is possible here
    if op_x ~= op_z then
      emit(ctx, 'mov  '..op_x..', '..op_z)
    end
    emit(ctx, instr..' '..op_y..', '..op_z)
  else
    -- Have to push something otherwise, and destination must be a register
    emit(ctx, 'push %rax')
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, instr..' '..op_y..', %rax')
    emit(ctx, 'mov  %rax, '..op_z)
    emit(ctx, 'pop  %rax')
  end
end

local function emit_comparison(ctx, set_instr, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_z == 'literal' then error('Invalid instruction') end
  if type_x == 'register' or type_z == 'register' then
    -- Either is a register, so only a single memory reference is possible here
    emit(ctx, 'cmp  '..op_x..', '..op_y)
  else
    -- Have to push something otherwise
    emit(ctx, 'push %rax')
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'cmp  %rax, '..op_y)
    emit(ctx, 'pop  %rax')
  end
  -- Clear result and set based on condition
  emit(ctx, 'mov  $0, '..op_z)
  emit(ctx, set_instr..' '..reg_byte_form(op_z, type_z))
end

local function emit_jump_comparison(ctx, jump_instr, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  -- Destination can't be a literal, that doesn't make any damn sense
  if type_x == 'literal' and type_y == 'literal' then error('Invalid instruction') end
  if type_x == 'register' or type_z == 'register' then
    -- Either is a register, so only a single memory reference is possible here
    emit(ctx, 'cmp  '..op_x..', '..op_y)
  else
    -- Have to push something otherwise
    emit(ctx, 'push %rax')
    emit(ctx, 'mov  '..op_x..', %rax')
    emit(ctx, 'cmp  %rax, '..op_y)
    emit(ctx, 'pop  %rax')
  end
  -- Jump based on condition
  emit(ctx, jump_instr..' '..convert_label(ctx, ir_stmt.label_name))
end

local function convert_label(ctx, ir_name)
  return '_s'..ctx.subroutine_id..ir_name
end

-- (e)mit (s)tatement (t)able
local est = {}

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
  -- This implementation is most efficient if op_x and op_z are both r0; hence, if division is the
  -- first element of its statement.
  -- Though, it's pretty marginal compared to how many cycles integer division itself takes.
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  if type_z == 'literal' then error('Invalid instruction') end
  -- Backup %rax,%rdx if applicable
  -- This backup flow control has an interesting pattern. We only push registers that would be
  -- clobbered, and aren't the destination.
  if op_z ~= '%rax' then
    emit(ctx, 'push %rax')
  end
  if op_z ~= '%rdx' then
    emit(ctx, 'push %rdx')
  end
  -- Place our dividend in %rax
  if op_x ~= '%rax' then
    emit(ctx, 'mov  '..op_x..', %rax')
  end
  -- Sign extend %rax into %rdx.
  -- This clobbers %rdx, and kills the crab.
  emit(ctx, 'cqto')
  -- Divide [%rdx:%rax] by the divisor (op_y)
  emit(ctx, 'idivq '..op_y)
  -- Move the quotient into our destination (op_z)
  if op_z ~= '%rax' then
    emit(ctx, 'mov  %rax, '..op_z)
  end
  -- Restore %rax,%rdx if applicable
  if op_z ~= '%rdx' then
    emit(ctx, 'pop  %rdx')
  end
  if op_z ~= '%rax' then
    emit(ctx, 'pop  %rax')
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

function est.salloc(ctx, ir_stmt)
  emit_stack_alloc(ctx, ir_stmt.size)
end
function est.sfree(ctx, ir_stmt)
  emit_stack_dealloc(ctx, ir_stmt.size)
end

function est.begincall(ctx, ir_stmt)
  local call_data = { }
  call_data.args_size = ir_stmt.size
  -- Clobbered argument registers should be saved and recorded here
  -- ...
  -- Allocate stack argument space, save beginning
  call_data.args_index = ctx.stack_index
  emit_stack_alloc(ctx, call_data.args_size)
  -- Save call data for future endcall
  table.insert(ctx.call_data_stack, call_data)
end
function est.endcall(ctx, ir_stmt)
  local call_data = table.remove(ctx.call_data_stack)
  if not call_data then error('No matching begincall instruction for endcall instruction') end
  -- Clobbered argument registers should be restored here
  -- ...
  -- Deallocate stack argument space
  emit_stack_dealloc(ctx, call_data.args_size)
end
function est.call(ctx, ir_stmt)
  emit(ctx, 'call '..ir_stmt.name)
end
function est.ret(ctx, ir_stmt)
  emit_epilogue(ctx)
end

function est.label(ctx, ir_stmt)
  emit_label(ctx, convert_label(ctx, ir_stmt.name))
end
function est.jmp(ctx, ir_stmt)
  emit(ctx, 'jmp '..convert_label(ctx, ir_stmt.label_name))
end
function est.jz(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register)
  if type_x == 'literal' then error('Invalid instruction') end
  emit(ctx, 'cmp $0, '..op_x)
  emit(ctx, 'je '..convert_label(ctx, ir_stmt.label_name))
end
function est.jnz(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register)
  if type_x == 'literal' then error('Invalid instruction') end
  emit(ctx, 'cmp $0, '..op_x)
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

function emit_statement(ctx, ir_stmt)
  local h = est[ir_stmt.type]
  if h then
    h(ctx, ir_stmt)
  else
    error('Unknown IR statement type `'..ir_stmt.type..'`')
  end
end

local function emit_subroutine(ctx, ir_subr)
  -- Emit header
  emit(ctx, '.globl '..ir_subr.name)
  emit(ctx, '.type '..ir_subr.name..', @function')
  emit_label(ctx, ir_subr.name)
  emit_prologue(ctx)
  -- We may need to do our own temporary register allocation/analysis in order to efficiently
  -- translate. After all, we track our own stack index. Perhaps this should eventually mean
  -- transforming the IR into something acceptable, rather than allocating registers on the fly.
  ctx.temp_count = ir_subr.meta.temp_count
  ctx.input_size = math.max(#ir_subr.arguments, #ir_subr.returns)
  ctx.call_data_stack = { }
  ctx.stack_index = 0
  -- Emit statements
  for i,ir_stmt in ipairs(ir_subr.statements) do
    emit_comment(ctx, ir.statement_string(ir_stmt))
    emit_statement(ctx, ir_stmt)
  end
  -- Emit default footer
  emit(ctx, 'mov $0, %rax')
  emit_epilogue(ctx)
end

local function generate(ir, outfile)
  local ctx = {
    outfile = outfile,
    subroutine_id = 0,
    temp_count = nil,
  }

  emit(ctx, '.text')
  emit_line(ctx)

  for i,ir_subr in ipairs(ir.subroutines) do
    emit_subroutine(ctx, ir_subr)
    emit_line(ctx)
    ctx.subroutine_id = ctx.subroutine_id + 1
  end

  emit(ctx, '.globl main')
  emit(ctx, '.type main, @function')
  emit_label(ctx, 'main')
  emit(ctx, 'push %rbp')
  emit(ctx, 'mov  %rsp, %rbp')
  emit(ctx, 'sub  $8, %rsp')
  emit(ctx, 'call my_module$main')
  emit(ctx, 'mov  -8(%rbp), %eax')
  emit(ctx, 'add  $8, %rsp')
  emit(ctx, 'mov  %rbp, %rsp')
  emit(ctx, 'pop  %rbp')
  emit(ctx, 'ret')
  emit_line(ctx)
end

return generate

