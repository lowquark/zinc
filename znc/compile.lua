
----------------------------------------------------------------------------------------------------
-- compile.lua - Compiles AST into IR code
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local cc = require 'cc'
local pprint = require 'pprint'

----------------------------------------------------------------------------------------------------
-- Error reporting

-- Reports a compilation error and aborts
local function report_error(ctx, str)
  io.write('Compilation error on line ?, col ?: '..str..'\n')
  io.write(debug.traceback())
  os.exit(4)
end

-- Places correctly pluralized units on the given number and returns the combined string
local function plz(n, singular, plural)
  if n == 1 then
    return tostring(n)..' '..singular
  else
    return tostring(n)..' '..plural
  end
end

----------------------------------------------------------------------------------------------------
-- Misc. allocations

-- Returns a subroutine-unique label
-- TODO: Why not program-unique?
local function new_label(ctx)
  local idx = ctx.label_index
  ctx.label_index = ctx.label_index + 1
  return 'l'..idx
end

----------------------------------------------------------------------------------------------------
-- Local variable / block allocations

-- Returns the number of machine words required to allocate the given type on the stack
function type_stack_size(ctx, var_type)
  if var_type.reference == false and var_type.hard_type == cc.type_int64 then
    -- int64, one machine word c:
    return 1
  end
  error('Failed to find stack size for type '..tostring(var_type))
end

-- Creates a local variable of the given type
-- Returns the constructed variable
--   ctx       : Compiler context
--   var_type  : cc.variable_type
--   -> cc.variable
function new_local(ctx, var_type)
  local block_stack = ctx.block_stack
  -- Calculate necessary storage space, in machine words
  local size = type_stack_size(ctx, var_type)
  -- Perform stack allocation
  local offset = block_stack:stack_alloc(size)
  -- Create an entry for a new stack local in the current subroutine
  local id = ir.create_local(ctx.subroutine, offset, size)
  -- Create official variable object
  local var = cc.variable(var_type, id)
  -- Register unnamed variable (for destruction)
  block_stack:add_variable(var)
  -- Return constructed variable
  return var
end

-- Adds the given local variable to the current scope under the given name
function add_to_scope(ctx, name, var)
  local block_stack = ctx.block_stack
  -- Ensure no shadowing occurs
  if block_stack:find_variable(name) then
    report_error(ctx, '`'..name..'` was already declared in this scope.')
  end
  -- Name in current scope
  block_stack:name_variable(name, var)
end

-- Finds the variable corresponding to the given name path
-- Returns the variable in question
-- Reports a compilation error on failure
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> cc.variable
local function find_variable(ctx, name_path)
  -- TODO: Add full paths to names? I guess full names can only correspond to members of a module.
  -- Hence, we should really only search a module scope stack in this case
  if #name_path > 1 then
    io.write('WARNING: Ignoring absolute path during search for variable '..name_path..'\n')
  end
  -- Just use last name in the path for now
  local name = name_path[#name_path]
  local var = ctx.block_stack:find_variable(name)
  -- Callers assume this never fails
  if not var then
    report_error(ctx, '`'..name_path..'` was not found in this scope.')
  end
  return var
end

-- Creates a new function declaration and adds it to the current module scope.
--   ctx      : Compiler context
--   name     : string
--   ast_func : ast.function_declaration
--   ir_subr  : ir.subroutine
--   -> cc.function_
local function new_function(ctx, name, ast_func, ir_subr)
  if ctx.module_scope[name] then
    report_error(ctx, 'Function `'..name..'` has already been declared in this scope.')
  else
    local new_decl = cc.function_(name, ast_func, ir_subr)
    ctx.module_scope[name] = new_decl
    return new_decl
  end
end

-- Returns the declaration corresponding to the named function
-- TODO: Stop punting on absolute paths
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> cc.function
local function find_function(ctx, name_path)
  local name_str = tostring(name_path)
  local func_decl = ctx.module_scope[name_str]
  if not func_decl then
    report_error(ctx, 'Function `'..name_str..'` was not found in this scope.')
  end
  return func_decl
end

----------------------------------------------------------------------------------------------------
-- Language type deduction

local function compute_type(ctx, ast_type_spec)
  if ast_type_spec.name_path[#ast_type_spec.name_path] == 'int64' then
    return cc.variable_type(false, cc.type_int64, false)
  else
    error('Cannot compute type!')
  end
end

----------------------------------------------------------------------------------------------------
-- IR generation

local emit

local new_temp_int64
local accept_only_int64

local lvalue_operand
local expression_operand
local assign_lvalue
local assign_lvalue_list

local eval_unop_expression
local eval_binop_expression
local eval_logical_expression

local eval_expression
local eval_expression_h = { }

local emit_statement
local emit_statement_h = { }

local emit_function

-- Appends an instruction onto the subroutine currently being generated
function emit(ctx, stmt)
  ir.add_statement(ctx.subroutine, stmt)
end

function new_temp_int64(ctx)
  local out_var = new_local(ctx, cc.variable_type(false, cc.type_int64, false))
  return cc.lvalue_proxy_variable(out_var, true), out_var
end

-- Call when only accepting int64 :p
function accept_only_int64(ctx, expr_proxy)
  assert(getmetatable(expr_proxy) == cc.expression_proxy_meta)

  local var_type

  if expr_proxy.__variant == 'literal' then
    -- We know literals are only numbers
    return
  elseif expr_proxy.__variant == 'lvalue' then
    -- Lvalue, make sure it references the correct type
    var_type = expr_proxy.lvalue.variable.type
  elseif expr_proxy.__variant == 'temporary' then
    -- Temporary, make sure it is the correct type
    var_type = expr_proxy.variable.type
  end

  -- Must be a plain int64
  if not ( var_type.hard_type == cc.type_int64 and var_type.reference == false ) then
    report_error(ctx, 'Only accepting int64 at this time')
  end
end

function lvalue_operand(lvalue_proxy)
  if lvalue_proxy.__variant == 'variable' then
    return lvalue_proxy.variable.ir_id
  elseif lvalue_proxy.__variant == 'array_element' then
    error('TODO')
  end
end

function expression_operand(expr_proxy)
  if expr_proxy.__variant == 'literal' then
    return ir.literal(expr_proxy.value)
  elseif expr_proxy.__variant == 'lvalue' then
    return lvalue_operand(expr_proxy.lvalue)
  end
end

function assign_lvalue(ctx, lvalue_proxy, expr_proxy)
  assert(getmetatable(lvalue_proxy) == cc.lvalue_proxy_meta)
  assert(getmetatable(expr_proxy) == cc.expression_proxy_meta)
  if lvalue_proxy.__variant == 'variable' then
    emit(ctx, ir.mov(lvalue_proxy.variable.ir_id, expression_operand(expr_proxy)))
  elseif lvalue_proxy.__variant == 'array_element' then
    error('TODO')
  end
end

function assign_lvalue_list(ctx, lvalue_proxies, expr_proxies)
  assert(#lvalue_proxies == #expr_proxies, 'List lengths must match.')
  for i=1,#lvalue_proxies do
    assign_lvalue(ctx, lvalue_proxies[i], expr_proxies[i])
  end
end

----------------------------------------------------------------------------------------------------
-- Expression generation

function eval_unop_expression(ctx, expr, op)
  -- Evaluate subexpression
  local in_expr = eval_expression(ctx, expr)
  accept_only_int64(ctx, in_expr);

  -- Create new temporary variable
  local out_lvalue, out_var = new_temp_int64(ctx)

  -- Submit, emit
  emit(ctx, op(out_var.ir_id, expression_operand(in_expr)))

  return cc.expression_proxy_lvalue(out_lvalue)
end

function eval_binop_expression(ctx, expr_a, expr_b, op)
  -- Evaluate subexpression
  local expr_a = eval_expression(ctx, expr_a)
  accept_only_int64(ctx, expr_a)

  -- Evaluate subexpression
  local expr_b = eval_expression(ctx, expr_b)
  accept_only_int64(ctx, expr_b)

  -- Create new slot
  local out_lvalue, out_var = new_temp_int64(ctx)

  -- Submit, emit
  emit(ctx, op(out_var.ir_id, expression_operand(expr_a), expression_operand(expr_b)))

  return cc.expression_proxy_lvalue(out_lvalue)
end

function eval_logical_expression(ctx, expr_a, expr_b, mode)
  local LABEL_B = new_label(ctx)
  local LABEL_END = new_label(ctx)

  -- Create destination temporary
  local out_lvalue, out_var = new_temp_int64(ctx)
  local op_out = out_var.ir_id;

  -- Evaluate subexpression A
  local expr_a = eval_expression(ctx, expr_a)
  accept_only_int64(ctx, expr_a)
  local op_a = expression_operand(expr_a);

  if mode == 'and' then
    -- If op_a is nonzero, jump to the evaluation of the next expression
    -- Otherwise, set op_out to zero, and jump to end (early exit, return zero)
    emit(ctx, ir.jnz(LABEL_B, op_a));
    emit(ctx, ir.mov(op_out, ir.literal(0)));
    emit(ctx, ir.jmp(LABEL_END));
  elseif mode == 'or' then
    -- If op_a is zero, jump to the evaluation of the next expression
    -- Otherwise, set op_out to one, and jump to end (early exit, return one)
    emit(ctx, ir.jz(LABEL_B, op_a));
    emit(ctx, ir.mov(op_out, ir.literal(1)));
    emit(ctx, ir.jmp(LABEL_END));
  else
    error('Bad mode')
  end

  -- LABEL_B
  emit(ctx, ir.label(LABEL_B))

  -- Evaluate subexpression B
  local expr_b = eval_expression(ctx, expr_b)
  accept_only_int64(ctx, expr_b)
  local op_b = expression_operand(expr_b);

  -- Set op_out to zero iff op_b is zero
  emit(ctx, ir.neq(op_out, op_b, ir.literal(0)))

  -- LABEL_END
  emit(ctx, ir.label(LABEL_END))

  return cc.expression_proxy_lvalue(out_lvalue)
end

-- Emits code which evaluates the given expression tree and stores it in a new slot
-- Returns the new slot
function eval_expression(ctx, expr)
  local h = eval_expression_h[expr.type]
  if h then
    return h(ctx, expr)
  else
    error('Unknown expression type `'..expr.type..'`\n')
  end
end

function eval_expression_h.integer(ctx, expr)
  return cc.expression_proxy_literal(tonumber(expr.value))
end

function eval_expression_h.negate(ctx, expr)
  return eval_unop_expression(ctx, expr.expression, ir.neg)
end

function eval_expression_h.binnot(ctx, expr)
  return eval_unop_expression(ctx, expr.expression, ir.bnot)
end

function eval_expression_h.lognot(ctx, expr)
  return eval_unop_expression(ctx, expr.expression, ir.lnot)
end

function eval_expression_h.add(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.add)
end

function eval_expression_h.sub(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.sub)
end

function eval_expression_h.mul(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.mul)
end

function eval_expression_h.div(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.div)
end

function eval_expression_h.cmpeq(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.eq)
end

function eval_expression_h.cmpneq(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.neq)
end

function eval_expression_h.cmplt(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.lt)
end

function eval_expression_h.cmpgt(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.gt)
end

function eval_expression_h.cmpleq(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.leq)
end

function eval_expression_h.cmpgeq(ctx, expr)
  return eval_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.geq)
end

function eval_expression_h.logand(ctx, expr)
  return eval_logical_expression(ctx, expr.expression_a, expr.expression_b, 'and')
end

function eval_expression_h.logor(ctx, expr)
  return eval_logical_expression(ctx, expr.expression_a, expr.expression_b, 'or')
end

function eval_expression_h.lvalue(ctx, expr)
  -- TODO: Index expressions => array element lvalue
  local var = find_variable(ctx, expr.lvalue.name_path)
  return cc.expression_proxy_lvalue(cc.lvalue_proxy_variable(var, false))
end

function eval_expression_h.call(ctx, expr)
  local func = find_function(ctx, expr.name_path)
  if #func.ast_function.returns > 0 then
    local dst_type = compute_type(ctx, func.ast_function.returns[1].type_specifier)

    -- Allocate temporary return storage for result
    -- TODO: lvalue_operands aren't supposed to be used as destination operands. But maybe they can
    -- be. I don't know.
    local lvalue = cc.lvalue_proxy_variable(new_local(ctx, dst_type), true)
    local lvalue_ops = { lvalue_operand(lvalue) }

    -- Compute arguments
    local expr_ops = { }
    for i,arg_expr in ipairs(expr.arguments) do
      local expr = eval_expression(ctx, arg_expr)
      expr_ops[i] = expression_operand(expr)
    end

    -- Emit call
    emit(ctx, ir.call(lvalue_ops, func.ir_subroutine.name, expr_ops))

    return cc.expression_proxy_lvalue(lvalue)
  else
    report_error(ctx, 'Cannot use function which returns no values in expression')
  end
end

----------------------------------------------------------------------------------------------------
-- Statement generation

function enter_block(ctx)
  return ctx.block_stack:enter_block()
end

function exit_block(ctx, b)
  -- This is where destructors would be called, e.g.:
  -- for i,l in ipairs(top_block.locals) do ... end

  ctx.block_stack:exit_block(b)
end

-- Generates IR code which implements an AST statement
function emit_statement(ctx, ast_stmt)
  local h = emit_statement_h[ast_stmt.type]
  if h then
    h(ctx, ast_stmt)
  else
    report_error(ctx, 'Unknown AST statement type `'..ast_stmt.type..'`')
  end
end

emit_statement_h['if'] = function(ctx, ast_stmt)
  -- Create block for temporaries
  -- Outside both statements seems to be a safe option.
  local b = enter_block(ctx)

  if ast_stmt.else_statement then
    -- If and else
    local LABEL_ELSE = new_label(ctx)
    local LABEL_END = new_label(ctx)

    -- Put conditional expression into new temporary
    local expr_var = eval_expression(ctx, ast_stmt.expression)
    accept_only_int64(ctx, expr_var)
    emit(ctx, ir.jz(LABEL_ELSE, expression_operand(expr_var)))

    -- Put if statement body
    emit_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.jmp(LABEL_END))

    -- Put else statement body
    emit(ctx, ir.label(LABEL_ELSE))
    emit_statement(ctx, ast_stmt.else_statement)
    emit(ctx, ir.label(LABEL_END))
  else
    -- Just if
    local LABEL_END = new_label(ctx)

    -- Put conditional expression into new temporary
    local expr_var = eval_expression(ctx, ast_stmt.expression)
    accept_only_int64(ctx, expr_var)
    emit(ctx, ir.jz(LABEL_END, expression_operand(expr_var)))

    -- Put if statement body
    emit_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(LABEL_END))
  end

  -- Clear temporaries
  exit_block(ctx, b)
end

emit_statement_h['return'] = function(ctx, ast_stmt)
  -- Create block for temporaries
  local b = enter_block(ctx)

  -- Assign function results to output argument registers
  for i,expr in ipairs(ast_stmt.expressions) do
    local expr = eval_expression(ctx, expr)
    accept_only_int64(ctx, expr)
    emit(ctx, ir.mov(ir.returnid(i - 1), expression_operand(expr)))
  end
  -- Don't forget to emit actual return statement
  emit(ctx, ir.ret())

  -- Clear temporaries
  exit_block(ctx, b)
end

function emit_statement_h.assignment(ctx, ast_stmt)
  -- Find or create lvalue variables
  local lvalues = { }
  local post_declarations = { }
  for i,ast_lvalue in ipairs(ast_stmt.lvalues) do
    local var

    if ast_lvalue.type == 'declaration' then
      -- Create a local, but don't add it to the current scope just yet. We don't want to allow
      -- use before the variable has been initialized.
      var = new_local(ctx, compute_type(ctx, ast_lvalue.type_specifier))
      -- Formally declare later
      table.insert(post_declarations, { ast_lvalue.name, var })
    elseif ast_lvalue.type == 'reference' then
      -- TODO: Index expressions! (WHAT?!)
      var = find_variable(ctx, ast_lvalue.name_path)
    end

    table.insert(lvalues, cc.lvalue_proxy_variable(var, false))
  end

  -- Create block for temporaries
  local b = enter_block(ctx)

  local first_expr = ast_stmt.expressions[1]

  if #ast_stmt.expressions == 1 and first_expr.type == 'call' then
    -- Call assignment
    -- Evaluate the call expression's lvalue operands
    local lvalue_ops = { }
    for i,lvalue in ipairs(lvalues) do
      lvalue_ops[i] = lvalue_operand(lvalue)
    end

    -- Evaluate the call expression's expressions and save operands
    local expr_ops = { }
    for i,ast_expr in ipairs(first_expr.arguments) do
      local expr = eval_expression(ctx, ast_expr)
      expr_ops[i] = expression_operand(expr)
    end

    -- Find this function
    local func = find_function(ctx, first_expr.name_path)

    -- Call now
    emit(ctx, ir.call(lvalue_ops, func.ir_subroutine.name, expr_ops))
  elseif #ast_stmt.expressions == 0 then
    -- Initialization assignment
    -- Just set each to zero
    for i,lvalue in ipairs(lvalues) do
      assign_lvalue(ctx, lvalue, cc.expression_proxy_literal(0))
    end
  elseif #ast_stmt.lvalues == #ast_stmt.expressions then
    -- Standard assignment
    local exprs = { }
    for i,expr in ipairs(ast_stmt.expressions) do
      exprs[i] = eval_expression(ctx, expr)
    end

    if #lvalues ~= 1 then
      -- Assign to temporaries, then move
      local temp_lvalues = { }
      local temp_exprs = { }
      for i,dst_var in ipairs(lvalues) do
        local var = new_local(ctx, dst_var.variable.type)
        local lvalue = cc.lvalue_proxy_variable(var, true)
        temp_lvalues[i] = lvalue
        temp_exprs[i] = cc.expression_proxy_lvalue(lvalue)
      end
      assign_lvalue_list(ctx, temp_lvalues, exprs)
      assign_lvalue_list(ctx, lvalues, temp_exprs)
    else
      -- No frills, just assign
      assign_lvalue_list(ctx, lvalues, exprs)
    end

    -- Good error.
    --error('Stop right there, criminal scum')
  else
    report_error(ctx, 'Invalid assignment')
  end

  -- Clear temporaries
  exit_block(ctx, b)

  for i,pd in ipairs(post_declarations) do
    add_to_scope(ctx, pd[1], pd[2])
  end
end

function emit_statement_h.block(ctx, ast_stmt)
  -- New block, new you
  local b = enter_block(ctx)

  -- Emit substatements
  for i,ast_substmt in ipairs(ast_stmt) do
    emit_statement(ctx, ast_substmt)
  end

  -- Bye bye block
  exit_block(ctx, b)
end

function emit_statement_h.call(ctx, ast_stmt)
  -- Create block for temporaries
  local b = enter_block(ctx)

  -- Evaluate the call expression's expressions and save operands
  local expr_ops = { }
  for i,ast_expr in ipairs(ast_stmt.arguments) do
    local expr = eval_expression(ctx, ast_expr)
    expr_ops[i] = expression_operand(expr)
  end

  -- Find this function
  local func = find_function(ctx, ast_stmt.name_path)

  -- Call now (no lvalues)
  emit(ctx, ir.call({ }, func.ir_subroutine.name, expr_ops))

  -- Clear temporaries
  exit_block(ctx, b)
end

----------------------------------------------------------------------------------------------------
-- Function generation

function emit_function(ctx, ast_func)
  -- Create a new IR subroutine for this function
  local subr = ir.subroutine('z$'..ast_func.name)
  -- Declare function in the current module scope (we will continue to generate it)
  local func_decl = new_function(ctx, ast_func.name, ast_func, subr)

  ctx.block_stack = cc.block_stack()

  -- Allocate argument registers and populate initial local scope
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local name = ast_arg_decl.name
    -- Compute the type from the AST type specifier
    local arg_type = compute_type(ctx, ast_arg_decl.type_specifier)
    -- Allocate this argument
    local ir_id = ir.create_argument(subr)
    -- Place in local scope
    local var = cc.variable(arg_type, ir_id)
    io.write('Adding '..tostring(var)..' to local scope under `'..name..'`\n')
    add_to_scope(ctx, name, var)
  end

  -- Allocate return registers
  for i,ast_arg_decl in ipairs(ast_func.returns) do
    ir.create_return(subr)
  end

  -- Initialize subroutine processing context
  ctx.subroutine = subr
  ctx.ast_function = ast_func

  -- Counter for unique label generation
  ctx.label_index = 0

  -- Location of next stack allocation
  ctx.stack_index = 0

  -- Index of next register allocation
  ctx.register_index = 0

  -- Map from local variable names to declarations
  ctx.local_scope = local_scope

  -- Read statements from AST
  emit_statement_h.block(ctx, ast_func.block)

  -- That's the subroutine
  ir.add_subroutine(ctx.program, subr)
end

local function compile(ast)
  local ctx = { }

  -- Map from absolute function names to declarations
  ctx.module_scope = { }

  -- Final object representing all compiled code
  -- TODO: Stop hardcoding main module name
  ctx.program = ir.program('my_module')

  -- Compile each function into a subroutine
  for i,ast_fdecl in ipairs(ast) do
    if ast_fdecl.type == 'module' then
      local ast_module = ast_fdecl
      for i,module_decl in ipairs(ast_module.declarations) do
        if module_decl.type == 'function' then
          emit_function(ctx, module_decl)
        end
      end
    end
  end
  return ctx.program
end

return compile

