
----------------------------------------------------------------------------------------------------
-- Generate x86-64 code from IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local lsra = require 'lsra'
local pprint = require 'pprint'

-- TODO: Large immediate values

local function list_contains(t, v)
  for i=1,#t do
    if t[i] == v then return true end
  end
  return false
end

-- These registers are used for memory reads and backing up values
--   %rax  - There's allegedly optimized instructions for this one
--   %rcx  - Shift operations always clobber this one

-- These registers directly map to IR registers
--   %rbx
--   %rdx
--   %r8
--   %r9
--   %r10
--   %r11

local oprand_reg_t = {
  '%rbx', -- <=> r0
  '%rdx', -- <=> r1
  '%r8',  -- <=> r2
  '%r9',  -- <=> r3
  '%r10', -- <=> r4
  '%r11', -- <=> r5
}

local reg_byte_form_t = {
  ['%rbx'] = '%bl',
  ['%rdx'] = '%dl',
  ['%r8'] = '%r8b',
  ['%r9'] = '%r9b',
  ['%r10'] = '%r10b',
  ['%r11'] = '%r11b',
}

-- (r)everse (s)et (i)nstruction (i)nequality (t)able
local rsiit = {
  ['sete'] = 'sete',
  ['setne'] = 'setne',
  ['setl'] = 'setg',
  ['setg'] = 'setl',
  ['setle'] = 'setge',
  ['setge'] = 'setle',
  -- Here lieth a mighty bug indeed
  --['setl'] = 'setge',
  --['setge'] = 'setl',
  --['setg'] = 'setle',
  --['setle'] = 'setg',
}

-- (r)everse (j)ump (i)nstruction (i)nequality (t)able
local rjiit = {
  ['je'] = 'je',
  ['jne'] = 'jne',
  ['jl'] = 'jg',
  ['jg'] = 'jl',
  ['jle'] = 'jge',
  ['jge'] = 'jle',
}

-- (e)mit (s)tatement (t)able
local emit_stmt = {}

----------------------------------------------------------------------------------------------------
-- Utilities

-- Computes the x86 instruction operand corresponding to the given IR register/literal
-- Returns the x86 operand and its corresponding type
local function operand(ctx, ir_name)
  -- IR operands always come like this
  -- TODO: This belongs in ir, I think!
  local typestr, intval = string.match(ir_name, '([rsab]?)(%d+)')
  intval = tonumber(intval)

  if typestr == 'r' then
    -- Regular register
    if intval < #oprand_reg_t then
      -- Use basic hardware register operand
      return oprand_reg_t[intval+1], 'register'
    end
  elseif typestr == 's' then
    -- Stack register
    -- Find the local's entry in the subroutine
    local local_data = ctx.subroutine.locals[intval+1]
    if not local_data then
      error('Local '..ir_name..' is not defined!')
    end
    -- Use rbp-relative operand corresponding to transformed stack location
    local rbp_offset_bytes = -8*(local_data.offset + local_data.size)
    return rbp_offset_bytes..'(%rbp)', 'memory'
  elseif typestr == 'a' then
    -- Argument register
    -- Use rbp-relative operand corresponding to argument's stack location
    local rbp_offset_bytes = 8*(ctx.subroutine.size_arguments - intval + 1)
    return rbp_offset_bytes..'(%rbp)', 'memory'
  elseif typestr == 'b' then
    -- Argument register
    -- Use rbp-relative operand corresponding to argument's stack location
    local rbp_offset_bytes = 8*(ctx.subroutine.size_arguments +
                                ctx.subroutine.size_returns - intval + 1)
    return rbp_offset_bytes..'(%rbp)', 'memory'
  elseif intval then
    -- Literal
    return '$'..intval, 'literal'
  end

  error('Failed to generate operand for `'..ir_name..'`')
end

-- Computes the least-significant-byte form of the given x86 operand
local function reg_byte_form(op, type)
  if type == 'register' then
    local r = reg_byte_form_t[op]
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
  emit(ctx, 'movq %rsp, %rbp')
end

local function emit_epilogue(ctx)
  emit(ctx, 'movq %rbp, %rsp')
  emit(ctx, 'pop  %rbp')
  emit(ctx, 'ret')
end

local function emit_stack_alloc(ctx, size)
  -- Allocate by subtracting from %rsp
  emit(ctx, 'subq $'..(size*8)..', %rsp')
  -- Track stack index (relative to %rbp)
  ctx.stack_index = ctx.stack_index + size
end

local function emit_stack_dealloc(ctx, size)
  -- Deallocate by adding to %rsp
  emit(ctx, 'addq $'..(size*8)..', %rsp')
  -- Track stack index (relative to %rbp)
  ctx.stack_index = ctx.stack_index - size
end

local function emit_push(ctx, op_x, type_x)
  emit(ctx, 'push '..op_x)
  ctx.stack_index = ctx.stack_index + 1
end

local function emit_pop(ctx, op_z, type_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  emit(ctx, 'pop '..op_z)
  ctx.stack_index = ctx.stack_index - 1
end

local function emit_mov(ctx, op_x, type_x, op_z, type_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  -- Optimize out useless moves
  if op_x == op_z then return end
  -- Move directly if possible
  if type_x == 'register' or type_z == 'register' then
    -- If either is a register, only one memory reference will be possible here
    emit(ctx, 'movq '..op_x..', '..op_z)
  else
    -- Into the accumulator it goes
    emit(ctx, 'movq '..op_x..', %rax')
    emit(ctx, 'movq %rax, '..op_z)
  end
end

local function emit_cmp(ctx, op_x, type_x, op_y, type_y, set_instr)
  -- TODO: This may be pretty common among instructions. Fix them all!
  -- Operand type configurations for cmp:
  --  register  register  ok
  --  register  literal   not ok - can swap
  --  register  memory    ok
  --  literal   register  ok
  --  literal   literal   not ok - can't swap
  --  literal   memory    ok
  --  memory    register  ok
  --  memory    literal   not ok - can swap
  --  memory    memory    not ok - can't swap
  -- Emit comparison instruction
  if type_x == 'literal' then
    if type_y == 'literal' then
      -- Into the accumulator it goes
      emit(ctx, 'movq '..op_x..', %rax')
      emit(ctx, 'cmpq '..op_y..', %rax')
    else
      -- Emit swapped comparison
      emit(ctx, 'cmpq '..op_x..', '..op_y)
      return 'swap'
    end
  elseif type_x == 'memory' and type_y == 'memory' then
    -- Into the accumulator it goes
    emit(ctx, 'movq '..op_x..', %rax')
    emit(ctx, 'cmpq '..op_y..', %rax')
  else
    -- Emit standard comparison
    emit(ctx, 'cmpq '..op_y..', '..op_x)
  end
end

local function emit_set(ctx, op_z, type_z, set_instr)
  assert(type_z ~= 'literal', 'Invalid instruction')
  -- Write destination
  if type_z == 'register' then
    emit(ctx, 'movq $0, '..op_z)
    emit(ctx, set_instr..' '..reg_byte_form(op_z, type_z))
  else
    emit(ctx, 'movq $0, %rax')
    emit(ctx, set_instr..' %al')
    emit(ctx, 'movq %rax, '..op_z)
  end
end

local function emit_stmt_binop(ctx, ir_stmt, instr)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  if type_z == 'register' then
    -- op_z is a register, so only a single memory reference will be possible here
    if op_x ~= op_z then
      emit(ctx, 'movq '..op_x..', '..op_z)
    end
    emit(ctx, instr..' '..op_y..', '..op_z)
  else
    -- Into the accumulator it goes
    emit(ctx, 'movq '..op_x..', %rax')
    emit(ctx, instr..' '..op_y..', %rax')
    emit(ctx, 'movq %rax, '..op_z)
  end
end

local function emit_stmt_comparison_set(ctx, ir_stmt, set_instr)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  -- If this is an inequality, reverse set instruction if cmp must be swapped
  if emit_cmp(ctx, op_x, type_x, op_y, type_y) == 'swap' then
    set_instr = rsiit[set_instr]
  end
  emit_set(ctx, op_z, type_z, set_instr)
end

local function emit_stmt_comparison_jump(ctx, ir_stmt, jump_instr)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  -- If this is an inequality, reverse jump instruction if cmp must be swapped
  if emit_cmp(ctx, op_x, type_x, op_y, type_y) == 'swap' then
    jump_instr = rjiit[jump_instr]
  end
  -- Jump based on condition
  emit(ctx, jump_instr..' '..convert_label(ctx, ir_stmt.label_name))
end

local function emit_stmt_comparison_jump_z(ctx, ir_stmt, jump_instr)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  -- If this is an inequality, reverse jump instruction if cmp must be swapped
  if emit_cmp(ctx, op_x, type_x, '$0', 'literal') == 'swap' then
    jump_instr = rjiit[jump_instr]
  end
  -- Jump based on condition
  emit(ctx, jump_instr..' '..convert_label(ctx, ir_stmt.label_name))
end

function emit_statement(ctx, ir_stmt)
  local h = emit_stmt[ir_stmt.type]
  if h then
    h(ctx, ir_stmt)
  else
    error('Unknown IR statement type `'..ir_stmt.type..'`')
  end
end

function emit_stmt.mov(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
end

function emit_stmt.neg(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  emit(ctx, 'negq '..op_z)
end

function emit_stmt.bnot(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  emit(ctx, 'notq '..op_z)
end

function emit_stmt.lnot(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  emit_mov(ctx, op_x, type_x, op_z, type_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  emit(ctx, 'cmp  $0, '..op_z)
  emit(ctx, 'movq $0, '..op_z)
  emit(ctx, 'sete '..reg_byte_form(op_z, type_z))
end

function emit_stmt.add(ctx, ir_stmt)
  emit_stmt_binop(ctx, ir_stmt, 'addq')
end

function emit_stmt.sub(ctx, ir_stmt)
  emit_stmt_binop(ctx, ir_stmt, 'subq')
end

function emit_stmt.mul(ctx, ir_stmt)
  emit_stmt_binop(ctx, ir_stmt, 'imul')
end

function emit_stmt.div(ctx, ir_stmt)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  local op_y, type_y = operand(ctx, ir_stmt.register_y)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  -- idiv has no encodings with an immediate divisor
  if type_y == 'literal' then
    emit(ctx, 'movq '..op_x..', %rax')
    emit(ctx, 'movq '..op_y..', %rcx')
    -- Backup %rdx if it will not be used to return
    if op_z ~= '%rdx' then
      emit_push(ctx, '%rdx', 'register')
    end
    -- Sign extend %rax into %rdx
    -- This kills the crab.
    emit(ctx, 'cqto')
    -- Divide [%rdx:%rax] by %rcx
    emit(ctx, 'idivq %rcx')
    emit(ctx, 'movq %rax, '..op_z)
    if op_z ~= '%rdx' then
      emit_pop(ctx, '%rdx', 'register')
    end
  elseif op_y == '%rdx' then
    -- We'll need to divide by %rcx instead.
    emit(ctx, 'movq %rdx, %rcx')
    -- Place our dividend in %rax
    emit(ctx, 'movq '..op_x..', %rax')
    -- Sign extend %rax into %rdx
    -- This kills the crab.
    emit(ctx, 'cqto')
    -- Divide [%rdx:%rax] by %rcx
    emit(ctx, 'idivq %rcx')
    emit(ctx, 'movq %rax, '..op_z)
    emit(ctx, 'movq %rcx, %rdx')
  else
    -- Backup %rdx if it will not be used to return
    if op_z ~= '%rdx' then
      emit(ctx, 'movq %rdx, %rcx')
    end
    -- Place our dividend in %rax
    emit(ctx, 'movq '..op_x..', %rax')
    -- Sign extend %rax into %rdx
    -- This kills the crab.
    emit(ctx, 'cqto')
    -- Divide [%rdx:%rax] by (op_y)
    emit(ctx, 'idivq '..op_y)
    emit(ctx, 'movq %rax, '..op_z)
    if op_z ~= '%rdx' then
      emit(ctx, 'movq %rcx, %rdx')
    end
  end
end

function emit_stmt.eq(ctx, ir_stmt)
  emit_stmt_comparison_set(ctx, ir_stmt, 'sete')
end

function emit_stmt.neq(ctx, ir_stmt)
  emit_stmt_comparison_set(ctx, ir_stmt, 'setne')
end

function emit_stmt.lt(ctx, ir_stmt)
  emit_stmt_comparison_set(ctx, ir_stmt, 'setl')
end

function emit_stmt.gt(ctx, ir_stmt)
  emit_stmt_comparison_set(ctx, ir_stmt, 'setg')
end

function emit_stmt.leq(ctx, ir_stmt)
  emit_stmt_comparison_set(ctx, ir_stmt, 'setle')
end

function emit_stmt.geq(ctx, ir_stmt)
  emit_stmt_comparison_set(ctx, ir_stmt, 'setge')
end

function emit_stmt.str(ctx, ir_stmt)
  local op_w, type_w = operand(ctx, ir_stmt.register_w)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  emit(ctx, 'movq '..op_x..', ('..op_w..')')
end

function emit_stmt.ldr(ctx, ir_stmt)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  local op_x, type_x = operand(ctx, ir_stmt.register_x)
  emit(ctx, 'movq ('..op_x..'), '..op_z)
end

function emit_stmt.call(ctx, ir_stmt)
  local target_sub = ir.find_subroutine(ctx.program, ir_stmt.name)
  if not target_sub then
    error('Subroutine '..ir_stmt.name..' not found')
  end

  local return_ops = { }
  local return_types = { }
  local argument_ops = { }
  local argument_types = { }

  for i,reg in ipairs(ir_stmt.return_regs) do
    return_ops[i], return_types[i] = operand(ctx, reg)
  end

  for i,reg in ipairs(ir_stmt.argument_regs) do
    argument_ops[i], argument_types[i] = operand(ctx, reg)
  end

  -- Save all possibly clobbered registers
  -- TODO: Only save live registers!
  local saved_regs = { }
  for i=1,#oprand_reg_t do
    local reg = oprand_reg_t[i]
    if not list_contains(return_ops, reg) then
      emit_push(ctx, reg, 'register')
      saved_regs[#saved_regs+1] = reg
    end
  end

  -- Allocate stack argument space
  local arg_stack_idx = ctx.stack_index
  local input_size = #ir_stmt.return_regs + #ir_stmt.argument_regs
  if input_size > 0 then
    emit_stack_alloc(ctx, input_size)
  end

  -- Push arguments
  for i=1,#argument_ops do
    local rbp_offset_bytes = -8*(arg_stack_idx + i + #ir_stmt.return_regs)
    emit_mov(ctx, argument_ops[i], argument_types[i], rbp_offset_bytes..'(%rbp)', 'memory')
  end

  -- Emit actual call
  emit(ctx, 'call '..ir_stmt.name)

  -- Grab return values
  for i=1,#ir_stmt.return_regs do
    if return_ops[i] then
      local rbp_offset_bytes = -8*(arg_stack_idx + i)
      emit_mov(ctx, rbp_offset_bytes..'(%rbp)', 'memory', return_ops[i], return_types[i])
    end
  end

  -- Deallocate stack argument space
  if input_size > 0 then
    emit_stack_dealloc(ctx, input_size)
  end

  -- Restore all previously pushed registers
  for i=#saved_regs,1,-1 do
    emit_pop(ctx, saved_regs[i], 'register')
  end
end

function emit_stmt.ret(ctx, ir_stmt)
  emit_epilogue(ctx)
end

function emit_stmt.label(ctx, ir_stmt)
  emit_label(ctx, convert_label(ctx, ir_stmt.name))
end

function emit_stmt.jmp(ctx, ir_stmt)
  emit(ctx, 'jmp '..convert_label(ctx, ir_stmt.label_name))
end

function emit_stmt.jeq(ctx, ir_stmt)
  emit_stmt_comparison_jump(ctx, ir_stmt, 'je')
end

function emit_stmt.jneq(ctx, ir_stmt)
  emit_stmt_comparison_jump(ctx, ir_stmt, 'jne')
end

function emit_stmt.jlt(ctx, ir_stmt)
  emit_stmt_comparison_jump(ctx, ir_stmt, 'jl')
end

function emit_stmt.jgt(ctx, ir_stmt)
  emit_stmt_comparison_jump(ctx, ir_stmt, 'jg')
end

function emit_stmt.jleq(ctx, ir_stmt)
  emit_stmt_comparison_jump(ctx, ir_stmt, 'jle')
end

function emit_stmt.jgeq(ctx, ir_stmt)
  emit_stmt_comparison_jump(ctx, ir_stmt, 'jge')
end

function emit_stmt.jz(ctx, ir_stmt)
  emit_stmt_comparison_jump_z(ctx, ir_stmt, 'je')
end

function emit_stmt.jnz(ctx, ir_stmt)
  emit_stmt_comparison_jump_z(ctx, ir_stmt, 'jne')
end

function emit_stmt.stackaddr(ctx, ir_stmt)
  local op_z, type_z = operand(ctx, ir_stmt.register_z)
  assert(type_z ~= 'literal', 'Invalid instruction')
  local ir_alloc = ctx.subroutine.locals[ir_stmt.index+1]
  local rbp_offset_bytes = -8*(ir_alloc.offset + ir_alloc.size)
  if type_z == 'register' then
    emit(ctx, 'movq %rbp, '..op_z)
    emit(ctx, 'addq $'..rbp_offset_bytes..', '..op_z)
  else
    emit(ctx, 'movq %rbp, %rax')
    emit(ctx, 'addq $'..rbp_offset_bytes..', %rax')
    emit(ctx, 'movq %rax, '..op_z)
  end
end

local function emit_subroutine(ctx, ir_subr)
  -- Initialize context
  ctx.subroutine = ir_subr
  ctx.stack_index = 0
  -- Emit header
  emit(ctx, '.globl '..ir_subr.name)
  emit(ctx, '.type '..ir_subr.name..', @function')
  emit_label(ctx, ir_subr.name)
  emit_prologue(ctx)
  -- Reserve stack space for this function (part of the prologue, really)
  if ir_subr.size_stack > 0 then
    emit_stack_alloc(ctx, ir_subr.size_stack)
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
  ctx.program = ir_prog
  ctx.outfile = outfile

  emit(ctx, '.text')
  emit_line(ctx)

  for i,ir_subr in ipairs(ir_prog.subroutines) do
    -- Allocate hardware registers
    io.write('Allocating hardware registers for IR subroutine: `'..ir_subr.name..'`\n')
    lsra(ir_subr, #oprand_reg_t)
    -- Print to console for shits and giggles
    io.write('\nResultant IR:\n')
    ir.dump_subroutine(ir_subr)
    io.write('\n')
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
  emit(ctx, 'movq %rsp, %rbp')
  emit(ctx, 'subq $8, %rsp')
  -- TODO: The module containing main needs to be specified as a compiler argument
  emit(ctx, 'call z$main')
  emit(ctx, 'movq -8(%rbp), %rax')
  emit(ctx, 'addq $8, %rsp')
  emit(ctx, 'movq %rbp, %rsp')
  emit(ctx, 'pop  %rbp')
  emit(ctx, 'ret')
  emit_line(ctx)
end

return generate

