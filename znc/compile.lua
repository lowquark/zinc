
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local cc = require 'cc'
local pprint = require 'pprint'

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
local function report_error(ctx, str)
  io.write('Compilation error on line ?, col ?: '..str..'\n')
  io.write(debug.traceback())
  os.exit(4)
end

-- TODO: All scope objects should be placed in language construction module
-- Duplicates the given scope table
local function copy_scope(scope)
  local new_scope = { }
  for k,v in pairs(scope) do
    new_scope[k] = scope[k]
  end
  return new_scope
end

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
-- Misc. allocations

-- Returns a subroutine-unique label
local function new_label(ctx)
  local idx = ctx.label_index
  ctx.label_index = ctx.label_index + 1
  return 'l'..idx
end

----------------------------------------------------------------------------------------------------
-- Local variable / block allocations

function type_stack_size(ctx, var_type)
  if var_type.reference == false and var_type.hard_type == cc.type_int64 then
    -- int64, one machine word c:
    return 1
  end
  error('Failed to find stack size for type '..tostring(var_type))
end

function new_block(ctx)
  local block_stack = ctx.block_stack
  local top_block = block_stack[#block_stack]
  local b

  if top_block then
    -- Create a new block, inheriting the top (current) block's stack index and scope
    b = {
      stack_index = top_block.stack_index,
      scope = copy_scope(top_block.scope),
      locals = { },
    }
  else
    -- Create a top-level block
    b = {
      stack_index = 0,
      scope = { },
      locals = { },
    }
  end

  -- Push this block
  block_stack[#block_stack+1] = b
end

function free_block(ctx, b)
  -- Ensure that there is a block to free
  local block_stack = ctx.block_stack
  assert(#block_stack > 0)

  -- Optionally ensure that we're freeing the expected block
  if b then assert(block_stack[#block_stack] == b) end

  -- This is where destructors would be called, e.g.:
  -- for i,l in ipairs(b.locals) do ... end

  -- Remove top block
  block_stack[#block_stack] = nil
end

-- Attempts to create a local variable in the current block
--   ctx       : Compiler context
--   var_type  : cc.variable_type
--   -> cc.variable
function new_local(ctx, var_type)
  -- Find top block
  local block_stack = ctx.block_stack
  assert(#block_stack > 0)
  local b = block_stack[#block_stack]

  -- Calculate necessary storage space, in machine words
  local size = type_stack_size(ctx, var_type)

  -- Allocate on stack
  local offset = b.stack_index
  b.stack_index = b.stack_index + size

  -- Create an entry for a new local in the current subroutine
  local id = ir.create_local(ctx.subroutine, offset, size)
  -- Create official variable object
  local var = cc.variable(var_type, id)

  -- Return constructed variable
  return var
end

function add_to_scope(ctx, name, var)
  -- Find top block
  local block_stack = ctx.block_stack
  assert(#block_stack > 0)
  local b = block_stack[#block_stack]

  -- Shadowing is expressly forbidden
  if b.scope[name] then
    report_error(ctx, '`'..name..'` was already declared in this scope.')
  end

  -- Associate this variable with the given name in the current scope
  b.scope[name] = var
end

-- Returns the declaration corresponding to the named variable
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> cc.variable
local function find_variable(ctx, name_path)
  -- Find top block
  local block_stack = ctx.block_stack
  assert(#block_stack > 0)
  local b = block_stack[#block_stack]

  if #name_path > 1 then
    io.write('WARNING: Ignoring absolute path during search for variable '..name_path..'\n')
  end

  local var = b.scope[name_path[1]]
  if var then
    return var
  else
    report_error(ctx, '`'..name_path..'` was not found in this scope.')
  end
end

-- Creates a new function declaration and adds it to the current module scope
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
local put_unop_expression
local put_binop_expression

local emit_expression
local emit_expression_h = { }

local put_statement
local put_statement_h = { }

-- Appends an instruction onto the subroutine currently being generated
function emit(ctx, stmt)
  ir.add_statement(ctx.subroutine, stmt)
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
    var_type = expr_proxy.lvalue.variable.variable_type
  elseif expr_proxy.__variant == 'temporary' then
    -- Temporary, make sure it is the correct type
    var_type = expr_proxy.variable.variable_type
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

function new_temp_int64(ctx)
  local out_var = new_local(ctx, cc.variable_type(false, cc.type_int64, false))
  return cc.lvalue_proxy_variable(out_var, true), out_var
end

function put_call(ctx, lvalues, func, exprs)
  local lvalue_ops = { }
  local expr_ops = { }

  -- TODO: lvalue_operands aren't supposed to be used as destination operands. But maybe they can
  -- be. I don't know.
  for i,lvalue in ipairs(lvalues) do
    lvalue_ops[i] = lvalue_operand(lvalue)
  end

  for i,expr in ipairs(exprs) do
    expr_ops[i] = expression_operand(expr)
  end

  emit(ctx, ir.call(lvalue_ops, func.ir_subroutine.name, expr_ops))
end

----------------------------------------------------------------------------------------------------
-- Expression generation

function put_unop_expression(ctx, expr, op)
  -- Evaluate subexpression
  local in_expr = emit_expression(ctx, expr)
  accept_only_int64(ctx, in_expr);

  -- Create new temporary variable
  local out_lvalue, out_var = new_temp_int64(ctx)

  -- Submit, emit
  emit(ctx, op(out_var.ir_id, expression_operand(in_expr)))

  return cc.expression_proxy_lvalue(out_lvalue)
end

function put_binop_expression(ctx, expr_a, expr_b, op)
  -- Evaluate subexpression
  local expr_a = emit_expression(ctx, expr_a)
  accept_only_int64(ctx, expr_a)

  -- Evaluate subexpression
  local expr_b = emit_expression(ctx, expr_b)
  accept_only_int64(ctx, expr_b)

  -- Create new slot
  local out_lvalue, out_var = new_temp_int64(ctx)

  -- Submit, emit
  emit(ctx, op(out_var.ir_id, expression_operand(expr_a), expression_operand(expr_b)))

  return cc.expression_proxy_lvalue(out_lvalue)
end

function put_logical_expression(ctx, expr_a, expr_b, mode)
  local LABEL_B = new_label(ctx)
  local LABEL_END = new_label(ctx)

  -- Create destination temporary
  local out_lvalue, out_var = new_temp_int64(ctx)
  local op_out = out_var.ir_id;

  -- Evaluate subexpression A
  local expr_a = emit_expression(ctx, expr_a)
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
  local expr_b = emit_expression(ctx, expr_b)
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
function emit_expression(ctx, expr)
  local h = emit_expression_h[expr.type]
  if h then
    return h(ctx, expr)
  else
    error('Unknown expression type `'..expr.type..'`\n')
  end
end

function emit_expression_h.integer(ctx, expr)
  return cc.expression_proxy_literal(tonumber(expr.value))
end

function emit_expression_h.negate(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.neg)
end

function emit_expression_h.binnot(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.bnot)
end

function emit_expression_h.lognot(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.lnot)
end

function emit_expression_h.add(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.add)
end

function emit_expression_h.sub(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.sub)
end

function emit_expression_h.mul(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.mul)
end

function emit_expression_h.div(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.div)
end

function emit_expression_h.cmpeq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.eq)
end

function emit_expression_h.cmpneq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.neq)
end

function emit_expression_h.cmplt(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.lt)
end

function emit_expression_h.cmpgt(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.gt)
end

function emit_expression_h.cmpleq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.leq)
end

function emit_expression_h.cmpgeq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.geq)
end

function emit_expression_h.logand(ctx, expr)
  return put_logical_expression(ctx, expr.expression_a, expr.expression_b, 'and')
end

function emit_expression_h.logor(ctx, expr)
  return put_logical_expression(ctx, expr.expression_a, expr.expression_b, 'or')
end

function emit_expression_h.lvalue(ctx, expr)
  -- TODO: Index expressions => array element lvalue
  local var = find_variable(ctx, expr.lvalue.name_path)
  return cc.expression_proxy_lvalue(cc.lvalue_proxy_variable(var, false))
end

function emit_expression_h.call(ctx, expr)
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
      local expr = emit_expression(ctx, arg_expr)
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

function emit_multi_copy(ctx, dst_vars, src_vars)
  for k=1,#dst_vars do
    accept_only_int64(ctx, dst_vars[k])
    accept_only_int64(ctx, src_vars[k])
    emit(ctx, ir.mov(dst_vars[k].ir_id, src_vars[k].ir_id))
  end
end

function emit_multi_move(ctx, dst_vars, src_vars)
  for k=1,#dst_vars do
    accept_only_int64(ctx, dst_vars[k])
    accept_only_int64(ctx, src_vars[k])
    emit(ctx, ir.mov(dst_vars[k].ir_id, src_vars[k].ir_id))
  end
end

function emit_multi_init(ctx, dst_lvalues)
end

-- Generates IR code which implements an AST statement
function put_statement(ctx, ast_stmt)
  local h = put_statement_h[ast_stmt.type]
  if h then
    h(ctx, ast_stmt)
  else
    report_error(ctx, 'Unknown AST statement type `'..ast_stmt.type..'`')
  end
end

put_statement_h['if'] = function(ctx, ast_stmt)
  -- Create block for temporaries
  -- Outside both statements seems to be a safe option.
  local b = new_block(ctx)

  if ast_stmt.else_statement then
    -- If and else
    local LABEL_ELSE = new_label(ctx)
    local LABEL_END = new_label(ctx)

    -- Put conditional expression into new temporary
    local expr_var = emit_expression(ctx, ast_stmt.expression)
    accept_only_int64(ctx, expr_var)
    emit(ctx, ir.jz(LABEL_ELSE, expression_operand(expr_var)))

    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.jmp(LABEL_END))

    -- Put else statement body
    emit(ctx, ir.label(LABEL_ELSE))
    put_statement(ctx, ast_stmt.else_statement)
    emit(ctx, ir.label(LABEL_END))
  else
    -- Just if
    local LABEL_END = new_label(ctx)

    -- Put conditional expression into new temporary
    local expr_var = emit_expression(ctx, ast_stmt.expression)
    accept_only_int64(ctx, expr_var)
    emit(ctx, ir.jz(LABEL_END, expression_operand(expr_var)))

    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(LABEL_END))
  end

  -- Clear temporaries
  free_block(ctx, b)
end

put_statement_h['return'] = function(ctx, ast_stmt)
  -- Create block for temporaries
  local b = new_block(ctx)

  -- Assign function results to output argument registers
  for i,expr in ipairs(ast_stmt.expressions) do
    local expr = emit_expression(ctx, expr)
    accept_only_int64(ctx, expr)
    emit(ctx, ir.mov(ir.returnid(i - 1), expression_operand(expr)))
  end
  -- Don't forget to emit actual return statement
  emit(ctx, ir.ret())

  -- Clear temporaries
  free_block(ctx, b)
end

function put_statement_h.assignment(ctx, ast_stmt)
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
  local b = new_block(ctx)

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
      local expr = emit_expression(ctx, ast_expr)
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
      exprs[i] = emit_expression(ctx, expr)
    end

    if #lvalues ~= 1 then
      -- Assign to temporaries, then move
      local temp_lvalues = { }
      local temp_exprs = { }
      for i,dst_var in ipairs(lvalues) do
        local var = new_local(ctx, dst_var.variable.variable_type)
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
  free_block(ctx, b)

  for i,pd in ipairs(post_declarations) do
    add_to_scope(ctx, pd[1], pd[2])
  end
end

function put_statement_h.block(ctx, ast_stmt)
  -- New block, new you
  local b = new_block(ctx)

  -- Emit substatements
  for i,ast_substmt in ipairs(ast_stmt) do
    put_statement(ctx, ast_substmt)
  end

  -- Bye bye block
  free_block(ctx, b)
end

function put_statement_h.call(ctx, ast_stmt)
  -- Create block for temporaries
  local b = new_block(ctx)

  -- Evaluate the call expression's expressions and save operands
  local expr_ops = { }
  for i,ast_expr in ipairs(ast_stmt.arguments) do
    local expr = emit_expression(ctx, ast_expr)
    expr_ops[i] = expression_operand(expr)
  end

  -- Find this function
  local func = find_function(ctx, ast_stmt.name_path)

  -- Call now (no lvalues)
  emit(ctx, ir.call({ }, func.ir_subroutine.name, expr_ops))

  -- Clear temporaries
  free_block(ctx, b)
end

----------------------------------------------------------------------------------------------------
-- Function generation

local function put_function(ctx, ast_func)
  -- Create a new IR subroutine for this function
  local subr = ir.subroutine('z$'..ast_func.name)
  -- Declare function in the current module scope (we will continue to generate it)
  local func_decl = new_function(ctx, ast_func.name, ast_func, subr)

  ctx.block_stack = { }
  -- Top-level block for this function (no stack allocations - arguments only)
  new_block(ctx)

  -- Allocate argument registers and populate initial local scope
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local name = ast_arg_decl.name
    -- Compute the type from the AST type specifier
    local arg_type = compute_type(ctx, ast_arg_decl.type_specifier)
    -- Allocate this argument
    local ir_id = ir.create_argument(subr)
    -- Place in local scope
    add_to_scope(ctx, name, cc.variable(arg_type, ir_id))
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
  put_statement_h.block(ctx, ast_func.block)

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
          put_function(ctx, module_decl)
        end
      end
    end
  end
  return ctx.program
end

return compile

