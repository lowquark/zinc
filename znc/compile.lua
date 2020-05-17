
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local lc = require 'lc'
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
  if var_type.reference == false and var_type.hard_type == lc.type_int64 then
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
--   var_type  : lc.variable_type
--   -> lc.variable
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
  local var = lc.variable(var_type, id)

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
--   -> lc.variable
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
--   -> lc.function_
local function new_function(ctx, name, ast_func, ir_subr)
  if ctx.module_scope[name] then
    report_error(ctx, 'Function `'..name..'` has already been declared in this scope.')
  else
    local new_decl = lc.function_(name, ast_func, ir_subr)
    ctx.module_scope[name] = new_decl
    return new_decl
  end
end

-- Returns the declaration corresponding to the named function
-- TODO: Stop punting on absolute paths
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> lc.function
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
    return lc.variable_type(false, lc.type_int64, false)
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
local function accept_only_int64(ctx, var)
  if not ( var.variable_type.hard_type == lc.type_int64 and
           var.variable_type.reference == false ) then
    report_error(ctx, 'Only accepting int64 at this time')
  end
end

function put_call(ctx, dst_vars, func, src_vars)
  local dst_ops = { }
  local src_ops = { }

  for i,dst_var in ipairs(dst_vars) do
    dst_ops[i] = dst_var.ir_id
  end

  for i,src_var in ipairs(src_vars) do
    src_ops[i] = src_var.ir_id
  end

  emit(ctx, ir.call(dst_ops, func.ir_subroutine.name, src_ops))
end

----------------------------------------------------------------------------------------------------
-- Expression generation

function put_unop_expression(ctx, expr, op)
  -- Evaluate subexpression
  local expr_var = emit_expression(ctx, expr)
  accept_only_int64(ctx, expr_var);

  -- Create new slot
  local out_var = new_local(ctx, lc.variable_type(false, lc.type_int64, false))
  accept_only_int64(ctx, out_var); 

  -- Submit, emit
  emit(ctx, op(out_var.ir_id, expr_var.ir_id))

  return out_var
end

function put_binop_expression(ctx, expr_a, expr_b, op)
  -- Evaluate subexpression
  local expr_a_var = emit_expression(ctx, expr_a)
  accept_only_int64(ctx, expr_a_var); 

  -- Evaluate subexpression
  local expr_b_var = emit_expression(ctx, expr_b)
  accept_only_int64(ctx, expr_b_var); 

  -- Create new slot
  local out_var = new_local(ctx, lc.variable_type(false, lc.type_int64, false))
  accept_only_int64(ctx, out_var); 

  -- Submit, emit
  emit(ctx, op(out_var.ir_id, expr_a_var.ir_id, expr_b_var.ir_id))

  return out_var
end

function put_logical_expression(ctx, expr_a, expr_b, mode)
  local LABEL_B = new_label(ctx)
  local LABEL_END = new_label(ctx)

  -- Create a new slot for result
  local out_var = new_local(ctx, lc.variable_type(false, lc.type_int64, false))
  accept_only_int64(ctx, out_var); 

  -- Evaluate subexpression A
  local expr_a_var = emit_expression(ctx, expr_a)
  accept_only_int64(ctx, expr_a_var); 

  local reg_out = out_var.ir_id;
  local reg_a = expr_a_var.ir_id;

  if mode == 'and' then
    -- If reg_a is nonzero, jump to the evaluation of the next expression
    -- Otherwise, set reg_out to zero, and jump to end (early exit, return zero)
    emit(ctx, ir.jnz(LABEL_B, reg_a));
    emit(ctx, ir.mov(reg_out, ir.literal(0)));
    emit(ctx, ir.jmp(LABEL_END));
  elseif mode == 'or' then
    -- If reg_a is zero, jump to the evaluation of the next expression
    -- Otherwise, set reg_out to one, and jump to end (early exit, return one)
    emit(ctx, ir.jz(LABEL_B, reg_a));
    emit(ctx, ir.mov(reg_out, ir.literal(1)));
    emit(ctx, ir.jmp(LABEL_END));
  else
    error('Bad mode')
  end

  -- LABEL_B
  emit(ctx, ir.label(LABEL_B))

  -- Evaluate subexpression B
  local expr_b_var = emit_expression(ctx, expr_b)
  accept_only_int64(ctx, expr_b_var); 

  local reg_b = expr_b_var.ir_id;

  -- Set reg_out to zero iff reg_b is zero
  emit(ctx, ir.neq(reg_out, reg_b, ir.literal(0)))

  -- LABEL_END
  emit(ctx, ir.label(LABEL_END))

  -- Our result is in var_z
  return out_var
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
  local out_var = new_local(ctx, lc.variable_type(false, lc.type_int64, false))
  -- Always int64, so just mov this value
  emit(ctx, ir.mov(out_var.ir_id, ir.literal(tonumber(expr.value))))
  return out_var
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
  -- TODO: Index expressions!
  return find_variable(ctx, expr.lvalue.name_path)
end

function emit_expression_h.call(ctx, expr)
  local func = find_function(ctx, expr.name_path)
  if #func.ast_function.returns > 0 then
    local dst_type = compute_type(ctx, func.ast_function.returns[1].type_specifier)

    -- Allocate return storage
    local dst_vars = { new_local(ctx, dst_type) }

    -- Compute arguments
    local src_vars = { }
    for i,arg_expr in ipairs(expr.arguments) do
      src_vars[i] = emit_expression(ctx, arg_expr)
    end

    -- Put other call stuff
    put_call(ctx, dst_vars, func, src_vars)

    return dst_vars[1]
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

function emit_multi_init(ctx, dst_vars)
  for k,dst_var in ipairs(dst_vars) do
    accept_only_int64(ctx, dst_var)
    emit(ctx, ir.mov(dst_var.ir_id, ir.literal(0)))
  end
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
    accept_only_int64(ctx, expr_var); 
    emit(ctx, ir.jz(LABEL_ELSE, expr_var.ir_id))

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
    accept_only_int64(ctx, expr_var); 
    emit(ctx, ir.jz(LABEL_END, expr_var.ir_id))

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
    local expr_var = emit_expression(ctx, expr)
    accept_only_int64(ctx, expr_var); 
    emit(ctx, ir.mov(ir.returnid(i - 1), expr_var.ir_id))
  end
  -- Don't forget to emit actual return statement
  emit(ctx, ir.ret())

  -- Clear temporaries
  free_block(ctx, b)
end

function put_statement_h.assignment(ctx, ast_stmt)
  -- Find or create lvalue variables
  local dst_vars = { }
  local post_declarations = { }
  for i,ast_lvalue in ipairs(ast_stmt.lvalues) do
    if ast_lvalue.type == 'declaration' then
      -- Create a local, but don't add it to the current scope just yet. We don't want to allow
      -- references before the variable has been initialized.
      dst_vars[i] = new_local(ctx, compute_type(ctx, ast_lvalue.type_specifier))
      -- Formally declare later
      table.insert(post_declarations, { ast_lvalue.name, dst_vars[i] })
    elseif ast_lvalue.type == 'reference' then
      -- TODO: Index expressions! (WHAT?!)
      dst_vars[i] = find_variable(ctx, ast_lvalue.name_path)
    end
  end

  -- Create block for temporaries
  local b = new_block(ctx)

  if #ast_stmt.lvalues == #ast_stmt.expressions then
    -- Evaluate RHS expression list
    local src_vars = { }
    for i,expr in ipairs(ast_stmt.expressions) do
      src_vars[i] = emit_expression(ctx, expr)
    end

    if #dst_vars ~= 1 then
      -- Assign to temporaries, then move
      local tmp_vars = { }
      for i,dst_var in ipairs(dst_vars) do
        tmp_vars[i] = new_local(ctx, dst_var.variable_type)
      end
      emit_multi_copy(ctx, tmp_vars, src_vars)
      emit_multi_move(ctx, dst_vars, tmp_vars)
    else
      -- No frills, just assign
      emit_multi_copy(ctx, dst_vars, src_vars)
    end
  elseif #ast_stmt.expressions == 0 then
    emit_multi_init(ctx, dst_vars)
  elseif #ast_stmt.expressions == 1 then
    local call_expr = ast_stmt.expressions[1]
    if call_expr.type == 'call' then
      -- Evaluate the call expression's argument expressions
      local src_vars = { }
      for i,expr in ipairs(call_expr.arguments) do
        src_vars[i] = emit_expression(ctx, expr)
      end
      -- This becomes a simple call, where lvalues are assigned to function results
      put_call(ctx, dst_vars, find_function(ctx, call_expr.name_path), src_vars)
    else
      report_error(ctx, 'Invalid assignment')
    end
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

  -- Evaluate the call expression's argument expressions
  local src_vars = { }
  for i,expr in ipairs(ast_stmt.arguments) do
    src_vars[i] = emit_expression(ctx, expr)
  end
  -- This becomes a simple call
  put_call(ctx, { }, find_function(ctx, ast_stmt.name_path), src_vars)

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

  pprint(ctx.block_stack)

  -- Allocate argument registers and populate initial local scope
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local name = ast_arg_decl.name
    -- Compute the type from the AST type specifier
    local arg_type = compute_type(ctx, ast_arg_decl.type_specifier)
    -- Allocate this argument
    local ir_id = ir.create_argument(subr)
    -- Place in local scope
    add_to_scope(ctx, name, lc.variable(arg_type, ir_id))
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

