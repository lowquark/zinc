
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

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
--  contiguity of the stack arguments.
--
--]]

----------------------------------------------------------------------------------------------------
-- Utilities

-- Places correctly pluralized units on the given number and returns their combined string
local function plz(n, singular, plural)
  if n == 1 then
    return tostring(n)..' '..singular
  else
    return tostring(n)..' '..plural
  end
end

-- Reports a compilation error and aborts
local function report_error(str)
  io.write('Compilation error on line ?, col ?: '..str..'\n')
  io.write(debug.traceback())
  os.exit(4)
end

-- Duplicates the given scope table
local function dupscope(scope)
  local new_scope = { }
  for k,v in pairs(scope) do
    new_scope[k] = scope[k]
  end
  return new_scope
end

-- Temporary registers are allocated SSA-style via simple counter
-- This requires post processing!
local function new_temp_reg(ctx)
  local idx = ctx.temp_index
  ctx.temp_index = ctx.temp_index + 1
  -- Record high water mark
  if ctx.temp_index > ctx.temp_index_max then
    ctx.temp_index_max = ctx.temp_index
  end
  return 'r'..idx
end

-- Returns a subroutine-unique label
local function newlabel(ctx)
  local idx = ctx.label_index
  ctx.label_index = ctx.label_index + 1
  return 'l'..idx
end

-- Returns the register which contains the given local variable
-- TODO: What happens to this function when we introduce stack-allocated structs?
-- TODO: What happens to this function when we introduce non-local variables?
local function find_variable(ctx, name)
  local reg = ctx.local_scope[name]
  if reg then
    return reg
  else
    report_error('`'..name..'` was not found in this scope.')
  end
end

-- Returns the subroutine which contains the given function path
-- TODO: Allowing module members to be referenced by absolute path necessitates merging this search
-- with the search for local variables!
local function find_call(ctx, name_path)
  local name = table.concat(name_path, '$')
  local subr = ctx.subroutines[name]
  if not subr then
    report_error('Function '..table.concat(name_path, ':')..' is undefined here.')
  end
  return subr
end

local function new_local_variable(ctx, type_spec, name)
  -- TODO: Acknowledge type specifier
  if ctx.local_scope[name] then
    report_error('`'..name..'` was already declared in this scope.')
  else
    -- Allocate a new temporary register to this variable
    local reg = new_temp_reg(ctx)
    ctx.local_scope[name] = reg
    io.write('local `'..name..'` is in register '..reg..'\n')
    return reg
  end
end

----------------------------------------------------------------------------------------------------
-- IR generation

local emit
local put_call
local put_expression
local put_statement

-- (p)ut (e)xpression (t)able
local pet = { }
-- (p)ut (s)tatement (t)able
local pst = { }

-- Appends an instruction onto the subroutine currently being generated
function emit(ctx, ir_stmt)
  table.insert(ctx.subroutine.statements, ir_stmt)
end

-- Emits the appropriate instructions to call a function
-- If return_regs is specified, stores the first n return values in the given registers
function put_call(ctx, name_path, argument_exprs, return_regs)
  -- Find the function in question
  local name = table.concat(name_path, '$')
  local target_subr = find_call(ctx, name_path)
  return_regs = return_regs or { }
  -- Validate the function call
  if #argument_exprs ~= #target_subr.arguments then
    report_error('Wrong number of arguments for call to '..table.concat(name_path, ':')..'(...)')
  end
  if #return_regs > #target_subr.returns then
    -- This is actually an internal error
    error('Too many return registers for function call')
  end
  for k=#return_regs+1,#target_subr.returns do
    return_regs[k] = '~'
  end
  -- Allocate temporary registers for argument expressions
  local argument_regs = { }
  for k,ast_expr in ipairs(argument_exprs) do
    argument_regs[k] = new_temp_reg(ctx)
  end
  -- Emit argument expressions
  for k,ast_expr in ipairs(argument_exprs) do
    put_expression(ctx, ast_expr, argument_regs[k])
  end
  -- Actually emit call instruction (generate.lua does the heavy lifting now)
  emit(ctx, ir.call(return_regs, name, argument_regs))
end

local function put_binop_expression(ctx, dst_reg, expr_a, expr_b, ir_op_fn)
  -- Emit first expression directly to destination
  put_expression(ctx, expr_a, dst_reg)
  -- Allocate a temporary register and emit second expression to it
  local tmp_reg = new_temp_reg(ctx)
  put_expression(ctx, expr_b, tmp_reg)
  -- Add temporary register to destination register
  emit(ctx, ir_op_fn(dst_reg, dst_reg, tmp_reg))
end

-- Emits the appropriate instructions to evaluate the given AST statement
function put_expression(ctx, expr, dst_reg)
  local h = pet[expr.type]
  if h then
    return h(ctx, expr, dst_reg)
  else
    io.write('Unknown expression type `'..expr.type..'`\n')
    os.exit(4)
  end
end

function pet.integer(ctx, expr, dst_reg)
  emit(ctx, ir.mov(dst_reg, expr.value))
end

function pet.negate(ctx, expr, dst_reg)
  put_expression(ctx, expr.expression, dst_reg)
  emit(ctx, ir.neg(dst_reg, dst_reg))
end

function pet.binnot(ctx, expr, dst_reg)
  put_expression(ctx, expr.expression, dst_reg)
  emit(ctx, ir.bnot(dst_reg, dst_reg))
end

function pet.lognot(ctx, expr, dst_reg)
  put_expression(ctx, expr.expression, dst_reg)
  emit(ctx, ir.lnot(dst_reg, dst_reg))
end

function pet.add(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.add)
end

function pet.sub(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.sub)
end

function pet.mul(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.mul)
end

function pet.div(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.div)
end

function pet.cmpeq(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.eq)
end

function pet.cmpneq(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.neq)
end

function pet.cmplt(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.lt)
end

function pet.cmpgt(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.gt)
end

function pet.cmpleq(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.leq)
end

function pet.cmpgeq(ctx, expr, dst_reg)
  put_binop_expression(ctx, dst_reg, expr.expression_a, expr.expression_b, ir.geq)
end

function pet.logand(ctx, expr, dst_reg)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  put_expression(ctx, expr.expression_a, dst_reg)
  -- If nonzero, jump to the evaluation of the next expression
  -- Otherwise, set result to zero, and jump to end
  emit(ctx, ir.jnz(lab1, dst_reg));
  emit(ctx, ir.mov(dst_reg, 0));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local tmp_reg = new_temp_reg(ctx)
  put_expression(ctx, expr.expression_b, tmp_reg)
  -- Set dst_reg to zero iff tmp_reg is zero
  emit(ctx, ir.neq(dst_reg, tmp_reg, 0))
  emit(ctx, ir.label(lab2))
end

function pet.logor(ctx, expr, dst_reg)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  put_expression(ctx, expr.expression_a, dst_reg)
  -- If zero, jump to the evaluation of the next expression
  -- Otherwise, set result to one, and jump to end
  emit(ctx, ir.jz(lab1, dst_reg));
  emit(ctx, ir.mov(dst_reg, 1));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local tmp_reg = new_temp_reg(ctx)
  put_expression(ctx, expr.expression_b, tmp_reg)
  -- Set dst_reg to zero iff tmp_reg is zero
  emit(ctx, ir.neq(dst_reg, tmp_reg, 0))
  emit(ctx, ir.label(lab2))
end

function pet.variable(ctx, expr, dst_reg)
  emit(ctx, ir.mov(dst_reg, find_variable(ctx, expr.name)))
end

function pet.call(ctx, expr, dst_reg)
  -- Function calls only yield their first return value to expressions
  put_call(ctx, expr.name_path, expr.arguments, { dst_reg })
end

function put_statement(ctx, ast_stmt)
  local h = pst[ast_stmt.type]
  if h then
    h(ctx, ast_stmt)
  else
    report_error('Unknown AST statement type `'..ast_stmt.type..'`')
  end
end

pst['if'] = function(ctx, ast_stmt)
  if ast_stmt.else_statement then
    local lab1 = newlabel(ctx)
    local lab2 = newlabel(ctx)
    -- Put conditional expression into new temporary
    local reg_exp = new_temp_reg(ctx)
    put_expression(ctx, ast_stmt.expression, reg_exp)
    emit(ctx, ir.jz(lab1, reg_exp))
    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.jmp(lab2))
    -- Put else statement body
    emit(ctx, ir.label(lab1))
    put_statement(ctx, ast_stmt.else_statement)
    emit(ctx, ir.label(lab2))
  else
    local lab1 = newlabel(ctx)
    -- Put conditional expression into new temporary
    local reg_exp = new_temp_reg(ctx)
    put_expression(ctx, ast_stmt.expression, reg_exp)
    emit(ctx, ir.jz(lab1, reg_exp))
    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(lab1))
  end
end

pst['return'] = function(ctx, ast_stmt)
  local expr_regs = {}
  if #ctx.ast_func.returns ~= #ast_stmt.expressions then
    report_error('Function returns '..plz(#ctx.ast_func.returns, 'value', 'values')..
                 ', but '..plz(#ast_stmt.expressions, 'was', 'were')..
                 ' provided')
  end
  -- TODO: A single return value can have the mov instruction optimized out
  -- Allocate temporary registers for this multiple assignment and put expressions
  for i,expr in ipairs(ast_stmt.expressions) do
    local tmp_reg = new_temp_reg(ctx)
    put_expression(ctx, expr, tmp_reg)
    expr_regs[#expr_regs+1] = tmp_reg
  end
  -- Copy expression results into function argument space
  for i,expr in ipairs(ast_stmt.expressions) do
    emit(ctx, ir.mov('i'..(i-1), expr_regs[i]))
  end
  -- Don't forget the return instruction!
  emit(ctx, ir.ret())
end

function pst.assignment(ctx, ast_stmt)
  local lvalue_regs = {}
  -- Validate assignment configuration
  local num_ex = #ast_stmt.expressions
  local num_lv = #ast_stmt.lvalues
  -- Allocate/find registers containing lvalues
  for i,ast_lvalue in ipairs(ast_stmt.lvalues) do
    if ast_lvalue.type == 'declaration' then
      local reg = new_local_variable(ctx, ast_lvalue.type_specifier, ast_lvalue.name)
      table.insert(lvalue_regs, reg)
    elseif ast_lvalue.type == 'reference' then
      if #ast_lvalue.name_path ~= 1 then
        report_error('Fully-scoped referential lvalues not supported')
      end
      if #ast_lvalue.index_exprs ~= 0 then
        report_error('Referential lvalues with index expressions not supported')
      end
      local name = ast_lvalue.name_path[1]
      local reg = find_variable(ctx, name)
      table.insert(lvalue_regs, reg)
    else
      report_error('Unknown lvalue type `'..ast_lvalue..'`')
    end
  end
  -- If expressions are given, evaluate and assign
  if num_ex > 0 then
    local first_expr = ast_stmt.expressions[1]
    -- If the expression list is a single call, expand its return values to the given lvalues
    if first_expr.type == 'call' and num_ex == 1 then
      local func = find_call(ctx, first_expr.name_path)
      if #func.returns < num_lv then
        report_error('Cannot assign '..plz(#func.returns, 'return value', 'return values')..
                     ' to '..plz(num_lv, 'lvalue', 'lvalues')..'.')
      end
      put_call(ctx, first_expr.name_path, first_expr.arguments, lvalue_regs)
    else
      if num_ex ~= num_lv then
        report_error('Cannot assign '..plz(num_ex, 'expression', 'expressions')..
                     ' to '..plz(num_lv, 'lvalue', 'lvalues')..'.')
      end
      -- TODO: A single assigned value can have the mov instruction optimized out
      -- Allocate temporary registers for this multiple assignment and put expressions
      local expr_regs = {}
      for i,ast_expr in ipairs(ast_stmt.expressions) do
        local tmp_reg = new_temp_reg(ctx)
        io.write('Assignment tmp_reg: '..tmp_reg..'\n')
        put_expression(ctx, ast_expr, tmp_reg)
        expr_regs[i] = tmp_reg
      end
      -- Copy expression results into lvalues
      -- Lua does this in reverse for some reason. `a, a = 0, 1` yields a = 0
      for i=1,num_lv do
        emit(ctx, ir.mov(lvalue_regs[i], expr_regs[i]))
      end
    end
  end
end

function pst.block(ctx, ast_stmt)
  -- Save current scope
  local local_scope_prev = ctx.local_scope
  ctx.local_scope = dupscope(ctx.local_scope)
  -- Emit substatements
  for i,ast_substmt in ipairs(ast_stmt) do
    put_statement(ctx, ast_substmt)
  end
  -- Restore scope
  ctx.local_scope = local_scope_prev
end

function pst.call(ctx, ast_stmt)
  -- Emit a call, without saving any return values
  put_call(ctx, ast_stmt.name_path, ast_stmt.arguments)
end

local function compile_subroutine(ctx, ast_func)
  local subr = ir.subroutine({ }, { }, { })
  -- Populate initial scope with argument registers
  local local_scope = { }
  local k = 0
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local_scope[ast_arg_decl.name] = 'i'..tostring(k)
    k = k + 1
    table.insert(subr.arguments, 'int64')
  end
  for i,ast_arg_decl in ipairs(ast_func.returns) do
    table.insert(subr.returns, 'int64')
  end
  -- Initialize subroutine context
  ctx.subroutine = subr
  ctx.ast_func = ast_func
  -- Counter / high water mark for the temp stack
  ctx.temp_index = 0
  ctx.temp_index_max = 0
  -- This function's scope -> IR register map
  ctx.local_scope = local_scope
  -- Counter for unique label generation
  ctx.label_index = 0
  -- Read statements from AST
  pst.block(ctx, ast_func.block)
  -- Temp count wasn't known before compilation
  subr.meta.temp_count = ctx.temp_index_max
  return subr
end

local function compile(ast)
  -- Top-level context
  local ctx = {
    -- List of subroutines defined thus far
    subroutines = { },
  }
  -- Final object representing all compiled code
  local ir = {}
  ir.subroutines = {}
  -- Find each function and compile it into a subroutine
  for i,ast_fdecl in ipairs(ast) do
    if ast_fdecl.type == 'module' then
      local ast_module = ast_fdecl
      for i,mdecl in ipairs(ast_module.declarations) do
        if mdecl.type == 'function' then
          local ast_func = mdecl
          local subr = compile_subroutine(ctx, ast_func)
          subr.name = table.concat(ast_module.name_path, '$')..'$'..ast_func.name
          ctx.subroutines[subr.name] = subr
          table.insert(ir.subroutines, subr)
        end
      end
    end
  end
  return ir
end

return compile

