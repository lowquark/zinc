
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local pprint = require 'pprint'

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

-- Duplicates the given scope table
local function dupscope(scope)
  local new_scope = { }
  for k,v in pairs(scope) do
    new_scope[k] = scope[k]
  end
  return new_scope
end

-- Temporary registers are allocated SSA via simple counter, which requires post processing
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
    error('`'..name..'` was not found in this scope.')
  end
end

-- Returns the subroutine which contains the given function path
-- TODO: Allowing module members to be referenced by absolute path necessitates merging this search
-- with the search for local variables!
local function find_call(ctx, name_path)
  local name = table.concat(name_path, '$')
  local subr = ctx.subroutines[name]
  if not subr then
    error('Function '..table.concat(name_path, ':')..' is undefined here.')
  end
  return subr
end

local function new_local_variable(ctx, type_spec, name)
  -- TODO: Acknowledge type specifier
  if ctx.local_scope[name] then
    error('`'..name..'` was already declared in this scope.')
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

-- (e)mit (e)xpression (t)able
local pet = { }
-- (e)mit (s)tatement (t)able
local pst = {}

-- Appends an instruction onto the subroutine currently being generated
function emit(ctx, ir_stmt)
  table.insert(ctx.subroutine.statements, ir_stmt)
end

-- Emits the appropriate instructions to call a function
-- If return_regs is specified, stores the first n return values in the given registers
function put_call(ctx, name_path, arglist, return_regs)
  -- Find the function in question
  local name = table.concat(name_path, '$')
  local target_subr = find_call(ctx, name_path)
  local target_arg_num = #target_subr.arguments
  local target_ret_num = #target_subr.returns
  -- Validate the function call
  if target_arg_num ~= #arglist then
    error('Wrong number of arguments for call to '..table.concat(name_path, ':')..'(...)')
  end
  if return_regs and #return_regs > target_ret_num then
    error('Too many return registers for function call')
  end
  -- As far as the IR is concerned, argument space is return value space
  local ir_arg_space = math.max(target_arg_num, target_ret_num)
  -- Create an appropriate argument buffer
  emit(ctx, ir.begincall(ir_arg_space))
  -- Push arguments onto stack
  for k,ast_expr in ipairs(arglist) do
    local exp_reg = put_expression(ctx, ast_expr)
    -- Emit assignment to this argument register
    emit(ctx, ir.mov('a'..(k-1), exp_reg))
  end
  -- Actually emit call instruction
  emit(ctx, ir.call(name))
  -- Move return values into first n destination registers
  if return_regs then
    for k=1,#return_regs do
      emit(ctx, ir.mov(return_regs[k], 'a'..(k-1)));
    end
  end
  -- Free argument buffer
  emit(ctx, ir.endcall())
end

-- Emits the appropriate instructions to evaluate the given AST statement
-- The result will be placed in the register at the top of the temporary stack
function put_expression(ctx, expr)
  local h = pet[expr.type]
  if h then
    return h(ctx, expr)
  else
    io.write('Unknown expression type `'..expr.type..'`\n')
    os.exit(4)
  end
end

function pet.integer(ctx, expr)
  local reg = new_temp_reg(ctx)
  emit(ctx, ir.mov(reg, expr.value))
  return reg
end

function pet.negate(ctx, expr)
  local reg = put_expression(ctx, expr.expression)
  emit(ctx, ir.neg(reg, reg))
  return reg
end

function pet.binnot(ctx, expr)
  local reg = put_expression(ctx, expr.expression)
  emit(ctx, ir.bnot(reg, reg))
  return reg
end

function pet.lognot(ctx, expr)
  local reg = put_expression(ctx, expr.expression)
  emit(ctx, ir.lnot(reg, reg))
  return reg
end

function pet.add(ctx, expr)
  -- Emit subexpressions
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  -- Add this instruction to the list
  emit(ctx, ir.add(reg_z, reg_x, reg_y))
  -- Return register of result
  return reg_z
end

function pet.sub(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.sub(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.mul(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.mul(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.div(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.div(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.cmpeq(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.eq(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.cmpneq(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.neq(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.cmplt(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.lt(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.cmpgt(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.gt(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.cmpleq(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.leq(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.cmpgeq(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  local reg_y = put_expression(ctx, expr.expression_b)
  emit(ctx, ir.geq(reg_z, reg_x, reg_y))
  return reg_z
end

function pet.logand(ctx, expr)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  -- If nonzero, jump to the evaluation of the next expression
  -- Otherwise, set result to zero, and jump to end
  emit(ctx, ir.jnz(lab1, reg_x));
  emit(ctx, ir.mov(reg_z, 0));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local reg_y = put_expression(ctx, expr.expression_b)
  -- Set reg_z to zero iff reg_y is zero
  emit(ctx, ir.neq(reg_z, reg_y, 0))
  emit(ctx, ir.label(lab2))
  return reg_z
end

function pet.logor(ctx, expr)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  -- If zero, jump to the evaluation of the next expression
  -- Otherwise, set result to one, and jump to end
  emit(ctx, ir.jz(lab1, reg_x));
  emit(ctx, ir.mov(reg_z, 1));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local reg_y = put_expression(ctx, expr.expression_b)
  -- Set reg_z to zero iff reg_y is zero
  emit(ctx, ir.neq(reg_z, reg_y, 0))
  emit(ctx, ir.label(lab2))
  return reg_z
end

function pet.variable(ctx, expr)
  local reg = new_temp_reg(ctx)
  emit(ctx, ir.mov(reg, find_variable(ctx, expr.name)))
  return reg
end

function pet.call(ctx, expr)
  -- Functions only yield their first result to expressions
  local return_reg = new_temp_reg(ctx)
  put_call(ctx, expr.name_path, expr.arguments, { return_reg })
  return return_reg
end

function put_statement(ctx, ast_stmt)
  local h = pst[ast_stmt.type]
  if h then
    h(ctx, ast_stmt)
  else
    error('Unknown AST statement type `'..ast_stmt.type..'`')
  end
end

pst['if'] = function(ctx, ast_stmt)
  if ast_stmt.else_statement then
    local lab1 = newlabel(ctx)
    local lab2 = newlabel(ctx)
    -- Put conditional expression
    local reg_exp = put_expression(ctx, ast_stmt.expression)
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
    -- Put conditional expression
    local reg_exp = put_expression(ctx, ast_stmt.expression)
    emit(ctx, ir.jz(lab1, reg_exp))
    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(lab1))
  end
end

pst['return'] = function(ctx, ast_stmt)
  local expr = ast_stmt.expression
  if expr then
    -- Put return expression
    local reg_x = put_expression(ctx, expr)
    emit(ctx, ir.mov('i0', reg_x))
  end
  emit(ctx, ir.ret())
end

function pst.assign(ctx, ast_stmt)
  local reg = put_expression(ctx, ast_stmt.expression)
  emit(ctx, ir.mov(find_variable(ctx, ast_stmt.name), reg))
end

function pst.assignment(ctx, ast_stmt)
  local dst_regs = {}
  local src_regs = {}
  -- Validate assignment configuration
  local num_ex = #ast_stmt.expressions
  local num_lv = #ast_stmt.lvalues
  -- Allocate/find registers containing lvalues
  for i,ast_lvalue in ipairs(ast_stmt.lvalues) do
    if ast_lvalue.type == 'declaration' then
      local reg = new_local_variable(ctx, ast_lvalue.type_specifier, ast_lvalue.name)
      table.insert(dst_regs, reg)
    elseif ast_lvalue.type == 'reference' then
      pprint(ast_lvalue)
      if #ast_lvalue.name_path ~= 1 then
        error('Fully-scoped referential lvalues not supported')
      end
      if #ast_lvalue.index_exprs ~= 0 then
        error('Referential lvalues with index expressions not supported')
      end
      local name = ast_lvalue.name_path[1]
      local reg = find_variable(ctx, name)
      table.insert(dst_regs, reg)
    else
      error('Unknown lvalue type `'..ast_lvalue..'`')
    end
  end
  -- If expressions are given, evaluate and assign
  if num_ex > 0 then
    local first_expr = ast_stmt.expressions[1]
    -- If the expression list is a single call, expand its return values to the given lvalues
    if first_expr.type == 'call' and num_ex == 1 then
      local func = find_call(ctx, first_expr.name_path)
      if #func.returns < num_lv then
        error('Cannot assign '..#func.returns..' return value(s) to '..num_lv..' lvalue(s).')
      end
      put_call(ctx, first_expr.name_path, first_expr.arguments, dst_regs)
    else
      if num_ex ~= num_lv then
        error('Cannot assign '..num_ex..' expression(s) to '..num_lv..' lvalue(s).')
      end
      -- Evaluate expressions and assign to temporaries for great swapping/justice
      for i,ast_expr in ipairs(ast_stmt.expressions) do
        local src_reg = put_expression(ctx, ast_expr)
        table.insert(src_regs, src_reg)
      end
      -- Move temporaries into lvalues
      -- Lua does this in reverse for some reason. `a, a = 0, 1` yields a = 0
      for i=1,num_lv do
        local src_reg = src_regs[i]
        local dst_reg = dst_regs[i]
        emit(ctx, ir.mov(dst_reg, src_reg))
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

