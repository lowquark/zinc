
--------------------------------------------------------------------------------
-- Compile AST into IR
--------------------------------------------------------------------------------

local ir = require 'ir'

--[[
--
--  A note about nested call expressions:
--
--  begincall 4                 begincall 4
--  arg0 = x                    arg0 = x
--  arg1 = y                    arg1 = y
--  arg2 = call expression => | begincall 3
--  arg3 = z                  | arg0 = ...
--  endcall                   | arg1 = ...
--                            | call
--                            | tmp = arg2
--                            | endcall
--                              arg2 = tmp
--                              arg3 = z
--                              call
--                              endcall
--
--  1) The result of the call expression has no side-effects, and is stored in a temporary, just
--  like any other expression.
--
--  2) The begincall and endcall instructions give the implementation the opportunity to save and
--  restore physical registers that would be clobbered by passing of arguments in registers. This
--  will require stack space, but a push at begincall and a pop at endcall will not affect the
--  contiguity of the stack arguments. (Though, the implementation may want to track the true
--  address of the stack arguments being passed.)
--
--]]

-- Duplicates the given scope table
local function dupscope(scope)
  local new_scope = { }
  for k,v in pairs(scope) do
    new_scope[k] = scope[k]
  end
  return new_scope
end

local function pushtemp(ctx)
  local idx = ctx.temp_index
  ctx.temp_index = ctx.temp_index + 1
  -- Record high water mark
  if ctx.temp_index > ctx.temp_index_max then
    ctx.temp_index_max = ctx.temp_index
  end
  return 'r'..idx
end
local function poptemp(ctx)
  ctx.temp_index = ctx.temp_index - 1
end
local function cleartemps(ctx)
  ctx.temp_index = 0
end

local function pushstack(ctx)
  local idx = ctx.stack_index
  ctx.stack_index = ctx.stack_index + 1
  -- Record high water mark
  if ctx.stack_index > ctx.stack_index_max then
    ctx.stack_index_max = ctx.stack_index
  end
  return 's'..idx
end
local function popstack(ctx)
  ctx.stack_index = ctx.stack_index - 1
end

local function findlocal(ctx, name)
  local reg = ctx.local_scope[name]
  if reg then
    return reg
  else
    error('`'..name..'` was not found in this scope.')
  end
end

local function newlabel(ctx)
  local idx = ctx.label_index
  ctx.label_index = ctx.label_index + 1
  return 'l'..idx
end

local function find_call(ctx, name_path)
  local name = table.concat(name_path, '$')
  local subr = ctx.subroutines[name]
  if not subr then
    error('Function '..table.concat(name_path, ':')..' is undefined here.')
  end
  return subr
end

local function emit(ctx, ir_stmt)
  table.insert(ctx.subroutine.statements, ir_stmt)
end

local emit_expression

local function emit_call(ctx, name_path, arglist, catch_return)
  -- Find the function in question
  local name = table.concat(name_path, '$')
  local target_subr = find_call(ctx, name_path)
  if #target_subr.arguments ~= #arglist then
    error('Wrong number of arguments for call to '..table.concat(name_path, ':')..'(...)')
  end
  if catch_return and size_returns == 0 then
    error(table.concat(name_path, ':')..
          '(...) cannot be used in an expression; it has no return values.')
  end
  -- Create an appropriate argument bank
  local size_args = #target_subr.arguments
  local size_returns = #target_subr.returns
  local size_total = math.max(size_args, size_returns)
  emit(ctx, ir.begincall(size_total))
  -- Push arguments onto stack
  local arg_index = 0
  for k,ast_expr in ipairs(arglist) do
    local exp_reg = emit_expression(ctx, ast_expr)
    -- Emit assignment to this argument register
    local arg_index = k - 1
    local arg_reg = 'a'..arg_index
    arg_index = arg_index + 1
    emit(ctx, ir.mov(arg_reg, exp_reg))
    -- Free exp_reg, it will be at the top of the temp stack
    -- JANK: Is there a better way to free/track temporary registers?
    poptemp(ctx)
  end
  -- Actually emit call instruction
  emit(ctx, ir.call(name))
  -- Optionally catch first return value
  local return_reg_tmp
  if catch_return then
    return_reg_tmp = pushtemp(ctx)
    -- Place in temporary
    emit(ctx, ir.mov(return_reg_tmp, 'a0'));
  end
  -- Clear stack
  emit(ctx, ir.endcall(size_total))
  return return_reg_tmp
end

-- (e)mit (e)xpression (t)able
local eet = { }

function eet.integer(ctx, expr)
  local reg = pushtemp(ctx)
  emit(ctx, ir.mov(reg, expr.value))
  return reg
end

function eet.negate(ctx, expr)
  local reg = emit_expression(ctx, expr.expression)
  emit(ctx, ir.neg(reg, reg))
  return reg
end

function eet.binnot(ctx, expr)
  local reg = emit_expression(ctx, expr.expression)
  emit(ctx, ir.bnot(reg, reg))
  return reg
end

function eet.lognot(ctx, expr)
  local reg = emit_expression(ctx, expr.expression)
  emit(ctx, ir.lnot(reg, reg))
  return reg
end

function eet.add(ctx, expr)
  -- Emit subexpressions
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  -- Add this instruction to the list
  emit(ctx, ir.add(reg_a, reg_a, reg_b))
  -- Free reg_b, it will be at the top of the temp stack
  poptemp(ctx)
  -- Return register of result
  return reg_a
end

function eet.sub(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.sub(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.mul(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.mul(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.div(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.div(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.cmpeq(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.eq(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.cmpneq(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.neq(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.cmplt(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.lt(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.cmpgt(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.gt(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.cmpleq(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.leq(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.cmpgeq(ctx, expr)
  local reg_a = emit_expression(ctx, expr.expression_a)
  local reg_b = emit_expression(ctx, expr.expression_b)
  emit(ctx, ir.geq(reg_a, reg_a, reg_b))
  poptemp(ctx)
  return reg_a
end

function eet.logand(ctx, expr)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  local reg_a = emit_expression(ctx, expr.expression_a)
  -- If nonzero, jump to the evaluation of the next expression
  -- Otherwise, the result is zero, and reg_a already contains zero, so jump to end
  emit(ctx, ir.jnz(lab1, reg_a));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local reg_b = emit_expression(ctx, expr.expression_b)
  -- Set reg_a to zero iff reg_b is zero
  emit(ctx, ir.neq(reg_a, reg_b, 0))
  emit(ctx, ir.label(lab2))
  poptemp(ctx)
  return reg_a
end

function eet.logor(ctx, expr)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  local reg_a = emit_expression(ctx, expr.expression_a)
  -- If zero, jump to the evaluation of the next expression
  -- Otherwise, set result to one, and jump to end
  emit(ctx, ir.jz(lab1, reg_a));
  emit(ctx, ir.mov(reg_a, 1));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local reg_b = emit_expression(ctx, expr.expression_b)
  -- Set reg_a to zero iff reg_b is zero
  emit(ctx, ir.neq(reg_a, reg_b, 0))
  emit(ctx, ir.label(lab2))
  poptemp(ctx)
  return reg_a
end

function eet.variable(ctx, expr)
  local reg = pushtemp(ctx)
  emit(ctx, ir.mov(reg, findlocal(ctx, expr.name)))
  return reg
end

function eet.call(ctx, expr)
  return emit_call(ctx, expr.name_path, expr.arguments, true)
end

emit_expression = function(ctx, expr)
  local h = eet[expr.type]
  if h then
    return h(ctx, expr)
  else
    io.write('Unknown expression type `'..expr.type..'`\n')
    os.exit(4)
  end
end

local emit_statement

-- IR (e)mit (s)tatement (t)able
local ir_est = {}

ir_est['if'] = function(ctx, ast_stmt)
  if ast_stmt.else_statement then
    local lab1 = newlabel(ctx)
    local lab2 = newlabel(ctx)
    local reg_exp = emit_expression(ctx, ast_stmt.expression)
    emit(ctx, ir.jz(lab1, reg_exp))
    emit_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.jmp(lab2))
    emit(ctx, ir.label(lab1))
    emit_statement(ctx, ast_stmt.else_statement)
    emit(ctx, ir.label(lab2))
  else
    local lab1 = newlabel(ctx)
    local reg_exp = emit_expression(ctx, ast_stmt.expression)
    emit(ctx, ir.jz(lab1, reg_exp))
    emit_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(lab1))
  end
end

ir_est['local'] = function(ctx, ast_stmt)
  local reg = pushstack(ctx)
  ctx.local_scope[ast_stmt.name] = reg
  io.write('local `'..ast_stmt.name..'` is in stack register '..reg..'\n')
end

ir_est['return'] = function(ctx, ast_stmt)
  local expr = ast_stmt.expression
  if expr then
    local reg_x = emit_expression(ctx, expr)
    emit(ctx, ir.mov('i0', reg_x))
  else
    local reg = pushtemp(ctx)
    emit(ctx, ir.mov(reg, 0))
  end
  emit(ctx, ir.ret())
end

function ir_est.assign(ctx, ast_stmt)
  local reg = emit_expression(ctx, ast_stmt.expression)
  emit(ctx, ir.mov(findlocal(ctx, ast_stmt.name), reg))
end

function ir_est.block(ctx, ast_stmt)
  -- Calculate total stack space required by this block
  local stack_size = 0
  for i,ast_substmt in ipairs(ast_stmt) do
    if ast_substmt.type == 'local' then
      stack_size = stack_size + 1
    end
  end

  io.write('this block will use '..stack_size..' words of stack space\n')

  local stack_index_prev
  local local_scope_prev

  if stack_size ~= 0 then
    emit(ctx, ir.salloc(stack_size))

    stack_index_prev = ctx.stack_index
    local_scope_prev = ctx.local_scope

    ctx.local_scope = dupscope(ctx.local_scope)
  end

  for i,ast_substmt in ipairs(ast_stmt) do
    emit_statement(ctx, ast_substmt)
  end

  if stack_size ~= 0 then
    emit(ctx, ir.sfree(stack_size))

    ctx.stack_index = stack_index_prev
    ctx.local_scope = local_scope_prev
  end
end

function ir_est.call(ctx, ast_stmt)
  emit_call(ctx, ast_stmt.name_path, ast_stmt.arguments, false)
end

emit_statement = function(ctx, ast_stmt)
  -- All temporary registers are free game at the beginning of a statement
  cleartemps(ctx)
  local h = ir_est[ast_stmt.type]
  if h then
    h(ctx, ast_stmt)
  else
    error('Unknown AST statement type `'..ast_stmt.type..'`')
  end
end

local function ir_compile_subroutine(ctx, ast_func)
  local subr = ir.subroutine({ }, { }, { })
  -- Populate initial scope with argument registers
  local local_scope = { }
  local k = 0
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local_scope[ast_arg_decl.name] = 'i'..tostring(k)
    k = k + 1
    table.insert(subr.arguments, 'int')
  end
  for i,ast_arg_decl in ipairs(ast_func.returns) do
    local_scope[ast_arg_decl.name] = 'i'..tostring(k)
    k = k + 1
    table.insert(subr.returns, 'int')
  end
  -- Initialize subroutine context
  ctx.subroutine = subr
  -- Counter / high water mark for the temp stack
  ctx.temp_index = 0
  ctx.temp_index_max = 0
  -- This function's scope -> IR register map
  ctx.local_scope = local_scope
  ctx.stack_index = 0
  ctx.stack_index_max = 0
  -- Counter for unique label generation
  ctx.label_index = 0
  -- Read statements from AST
  ir_est.block(ctx, ast_func.block, true)
  -- Temp count wasn't known before compilation
  subr.meta.temp_count = ctx.temp_index_max
  return subr
end

local function compile_ir(ast)
  local ctx = {
    -- Top-level context:
    -- List of subroutines defined thus far
    subroutines = { },
  }

  -- table.sort(ir.subroutines, function(sa, sb) return sa.name:lower() < sb.name:lower() end)

  local ir = {}
  ir.subroutines = {}

  for i,ast_fdecl in ipairs(ast) do
    if ast_fdecl.type == 'module' then
      local ast_module = ast_fdecl
      for i,mdecl in ipairs(ast_module.declarations) do
        if mdecl.type == 'function' then
          local ast_func = mdecl
          local subr = ir_compile_subroutine(ctx, ast_func)
          subr.name = table.concat(ast_module.name_path, '$')..'$'..ast_func.name
          ctx.subroutines[subr.name] = subr
          table.insert(ir.subroutines, subr)
        end
      end
    end
  end

  return ir
end

return compile_ir

