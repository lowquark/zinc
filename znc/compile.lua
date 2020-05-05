
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'

----------------------------------------------------------------------------------------------------
-- Utilities

-- Places correctly pluralized units on the given number and returns the combined string
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

-- Copies the given table recursively
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

-- Temporary registers are allocated via simple counter
-- For the results of temporary expressions this means the register is SSA
local function new_temp_reg(ctx)
  local idx = ctx.temp_index
  ctx.temp_index = ctx.temp_index + 1
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

-- Returns the subroutine corresponding to the given function path
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

-- Attempts to create a new local variable in the current scope
local function new_local_variable(ctx, type_spec, name)
  -- TODO: Acknowledge type specifier
  if ctx.local_scope[name] then
    report_error('`'..name..'` was already declared in this scope.')
  else
    -- Allocate a new temporary register to this variable (not SSA)
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
  local stmts = ctx.subroutine.statements
  stmts[#stmts+1] = ir_stmt
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
  -- The IR expects null registers for ignored return values. Lua can't store nil in a list, so we
  -- use '~' to denote the nil register. TODO: Put IR register names behind an API.
  for k=#return_regs+1,#target_subr.returns do
    return_regs[k] = '~'
  end
  -- Find/generate argument expressions
  local argument_regs = { }
  for k,ast_expr in ipairs(argument_exprs) do
    argument_regs[k] = put_expression(ctx, ast_expr)
  end
  -- Actually emit call instruction
  emit(ctx, ir.call(return_regs, name, argument_regs))
end

-- Emits the appropriate instructions to compute a simple unary arithmetic expression
local function put_unop_expression(ctx, expr, ir_op_fn)
  local reg_x = put_expression(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  emit(ctx, ir_op_fn(reg_z, reg_x))
  return reg_z
end

-- Emits the appropriate instructions to compute a simple binary arithmetic expression
local function put_binop_expression(ctx, expr_a, expr_b, ir_op_fn)
  local reg_x = put_expression(ctx, expr_a)
  local reg_y = put_expression(ctx, expr_b)
  local reg_z = new_temp_reg(ctx)
  emit(ctx, ir_op_fn(reg_z, reg_x, reg_y))
  return reg_z
end

-- Emits mov instructions to assign lvalue_regs := rvalue_regs, allocating temporaries as necessary
local function put_assignment(ctx, lvalue_regs, rvalue_regs)
  assert(#lvalue_regs == #rvalue_regs)
  local n = #lvalue_regs
  -- Make a copy for our own use
  rvalue_regs = deepcopy(rvalue_regs)
  -- Buffer lvalue registers that need buffering
  for i=1,n do
    local new_rvalue_reg
    for j=i+1,n do
      if lvalue_regs[i] == rvalue_regs[j] then
        -- This lvalue would be used after it has been assigned to!
        io.write(lvalue_regs[i]..' would be used in mov '..j..
                 ' after being assigned in mov '..i..'\n')
        if not new_rvalue_reg then
          -- Allocate a register to save this lvalue, but only once
          new_rvalue_reg = new_temp_reg(ctx)
          emit(ctx, ir.mov(new_rvalue_reg, rvalue_regs[j]))
        end
        -- Point to the copied value instead
        assert(rvalue_regs[j] ~= new_rvalue_reg)
        rvalue_regs[j] = new_rvalue_reg
      end
    end
  end
  -- Copy expression results into lvalues
  -- Lua does this in reverse for some reason. `a, a = 0, 1` yields a = 0
  for i=1,n do
    emit(ctx, ir.mov(lvalue_regs[i], rvalue_regs[i]))
  end
end

-- Emits instructions as necessary to evaluate the given AST expression
-- Returns a literal, or a register (not necessarily new) which contains the expression result
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
  return expr.value
end

function pet.negate(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.neg)
end

function pet.binnot(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.bnot)
end

function pet.lognot(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.lnot)
end

function pet.add(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.add)
end

function pet.sub(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.sub)
end

function pet.mul(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.mul)
end

function pet.div(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.div)
end

function pet.cmpeq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.eq)
end

function pet.cmpneq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.neq)
end

function pet.cmplt(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.lt)
end

function pet.cmpgt(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.gt)
end

function pet.cmpleq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.leq)
end

function pet.cmpgeq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.geq)
end

function pet.logand(ctx, expr)
  local lab1 = newlabel(ctx)
  local lab2 = newlabel(ctx)
  local reg_z = new_temp_reg(ctx)
  local reg_x = put_expression(ctx, expr.expression_a)
  -- If nonzero, jump to the evaluation of the next expression
  -- Otherwise, set reg_z to zero, and jump to end
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
  -- Otherwise, set reg_z to one, and jump to end
  emit(ctx, ir.jz(lab1, reg_x));
  emit(ctx, ir.mov(reg_z, 1));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local reg_y = put_expression(ctx, expr.expression_b)
  -- Set reg_z to zero iff reg_y is zero
  emit(ctx, ir.neq(reg_z, reg_y, 0))
  emit(ctx, ir.label(lab2))
  -- Our result is in reg_z
  return reg_z
end

function pet.variable(ctx, expr)
  return find_variable(ctx, expr.name)
end

function pet.call(ctx, expr)
  -- Function calls only yield their first return value to expressions
  local dst_reg = new_temp_reg(ctx)
  put_call(ctx, expr.name_path, expr.arguments, { dst_reg })
  return dst_reg
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
    local expr_reg = put_expression(ctx, ast_stmt.expression)
    emit(ctx, ir.jz(lab1, expr_reg))
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
    local expr_reg = put_expression(ctx, ast_stmt.expression)
    emit(ctx, ir.jz(lab1, expr_reg))
    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(lab1))
  end
end

pst['return'] = function(ctx, ast_stmt)
  if #ctx.ast_func.returns ~= #ast_stmt.expressions then
    report_error('Function returns '..plz(#ctx.ast_func.returns, 'value', 'values')..
                 ', but '..plz(#ast_stmt.expressions, 'was', 'were')..
                 ' provided')
  end
  -- Prepare for multiple assignment
  local n = #ctx.ast_func.returns
  local lvalue_regs = {}
  local rvalue_regs = {}
  for i=1,n do
    lvalue_regs[i] = 'i'..(i-1)
  end
  for i=1,n do
    rvalue_regs[i] = put_expression(ctx, ast_stmt.expressions[i])
  end
  put_assignment(ctx, lvalue_regs, rvalue_regs)
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
      report_error('Invalid lvalue type `'..ast_lvalue..'`')
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
      -- Prepare for multiple assignment
      local rvalue_regs = {}
      for i,ast_expr in ipairs(ast_stmt.expressions) do
        rvalue_regs[i] = put_expression(ctx, ast_expr, tmp_reg)
      end
      put_assignment(ctx, lvalue_regs, rvalue_regs)
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
  -- Emit a call, ignoring any return values
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
  -- This function's scope -> IR register map
  ctx.local_scope = local_scope
  -- Counter for unique label generation
  ctx.label_index = 0
  -- Read statements from AST
  pst.block(ctx, ast_func.block)
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

