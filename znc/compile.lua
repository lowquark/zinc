
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local pprint = require 'pprint'

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
-- Language declarations

local lang = { }

----------------------------------------------------------------------------------------------------
-- lang.variable

function lang.variable(name, variable_type, allocation)
  assert(name == nil or type(name) == 'string')
  assert(type(variable_type) == 'table')
  assert(type(allocation) == 'table')
  return { type = 'local',
           name = name,
           variable_type = variable_type,
           allocation = allocation }
end

----------------------------------------------------------------------------------------------------
-- lang.function_

function lang.function_(name, ast_func, ir_subr)
  assert(type(name) == 'string')
  assert(type(ast_func) == 'table')
  assert(type(ir_subr) == 'table')
  return { name = name,
           ast_function = ast_func,
           ir_subroutine = ir_subr }
end

----------------------------------------------------------------------------------------------------
-- lang.type

lang.type_int8 = { type = 'primitive', ptype = 'int8' }
lang.type_int16 = { type = 'primitive', ptype = 'int16' }
lang.type_int32 = { type = 'primitive', ptype = 'int32' }
lang.type_int64 = { type = 'primitive', ptype = 'int64' }

function lang.type_struct(size, atomic, complex, fields, access_list)
  assert(type(size) == 'number')
  assert(type(atomic) == 'boolean')
  assert(type(complex) == 'boolean')
  assert(type(fields) == 'table')
  assert(type(access_list) == 'table')
  return { type = 'struct',
           atomic = atomic,
           complex = complex,
           fields = deepcopy(fields),
           access_list = deepcopy(access_list) }
end

function lang.type_array(value_type, size)
  assert(type(value_type) == 'table')
  assert(type(size) == 'number')
  return { type = 'array',
           value_type = value_type,
           size = size }
end

function lang.type_reference(value_type)
  assert(type(value_type) == 'table')
  return { type = 'reference',
           value_type = value_type }
end

----------------------------------------------------------------------------------------------------
-- lang.struct_field

function lang.struct_field(field_name, field_type)
  assert(type(field_name) == 'string')
  assert(type(field_type) == 'table')
  return { name = field_name,
           type = field_type }
end

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
local function copy_scope(scope)
  local new_scope = { }
  for k,v in pairs(scope) do
    new_scope[k] = scope[k]
  end
  return new_scope
end

local bb = { }

local slot_meta = { __index = { } }

function bb.slot(value_type, allocation)
  return { value_type = value_type, allocation = allocation }
end

----------------------------------------------------------------------------------------------------
-- Allocations

-- Returns a subroutine-unique label
local function new_label(ctx)
  local idx = ctx.label_index
  ctx.label_index = ctx.label_index + 1
  return 'l'..idx
end

-- Stack storage is allocated via simple stacking
-- TODO: Re-implement unstacking at end of block scope (why was that removed?)
local function alloc_stack_space(ctx, size)
  local idx = ctx.stack_index
  ctx.stack_index = ctx.stack_index + size
  return idx, size
end

-- Conveniently allocates a single temporary register
local function new_temp_reg(ctx)
  local allocation = ctx.subroutine:alloc_register(1)
  return ir.register(allocation.first)
end

local function find_type(ctx, name_path)
  if type(name_path) == 'table' then
    if #name_path == 1 and name_path[1] == 'int64' then
      return lang.type_int64
    end
    report_error('Unknown type `'..name_path..'`.')
  elseif type(name_path) == 'string' then
    if name_path == 'int64' then
      return lang.type_int64
    end
    report_error('Unknown type `'..name_path..'`.')
  else
    error('')
  end
end

local function compute_type(ctx, ast_type_spec)
  local t = find_type(ctx, ast_type_spec.name_path)
  if ast_type_spec.const then
    -- Wrap type in const
    t = lang.type_const(t)
  end
  if ast_type_spec.quantity then
    -- Wrap type in array
    t = lang.type_array(t, ast_type_spec.quantity)
  end
  if ast_type_spec.reference then
    -- Wrap type in reference
    t = lang.type_reference(t)
  end
  return t
end

local function allocate_local(ctx, lang_type)
  if lang_type.type == 'array' then
    -- Allocate according to the array's value type
    -- TODO: Actually do this
    local val_type = lang_type.value_type
    if not (val_type.type and val_type.ptype == 'int64') then
      report_error('Array value types other than int64 are unsupported.')
    end
    local size_quadwords = lang_type.size * 1 -- sizeof(int64) in quadwords
    local offset, size = alloc_stack_space(ctx, size_quadwords)
    -- Allocate a new block of stack space
    return ctx.subroutine:alloc_stack(offset, size)
  else
    -- Allocate a new temporary register to this variable
    local size_quadwords = 1
    return ctx.subroutine:alloc_register(size_quadwords)
  end
end

-- Attempts to create a new local variable in the current scope
--   ctx       : Compiler context
--   name      : string
--   lang_type : lang.type
--   -> lang.variable
local function new_local_variable(ctx, name, lang_type)
  if ctx.local_scope[name] then
    report_error('`'..name..'` was already declared in this scope.')
  else
    -- Allocate this variable
    local allocation = allocate_local(ctx, lang_type)
    -- Pretty print
    if allocation.type == 'stack' then
      local first = tostring(allocation.offset)
      local last = tostring(allocation.offset + allocation.size - 1)
      io.write('local `'..name..'` is stack allocated on ['..first..', '..last..']\n')
    else
      local first = 'r'..allocation.first
      local last = 'r'..(allocation.first + allocation.count - 1)
      io.write('local `'..name..'` is register allocated on ['..first..', '..last..']\n')
    end
    -- Associate this declaration with this variable name in the current scope
    local new_decl = lang.variable(name, lang_type, allocation)
    ctx.local_scope[name] = new_decl
    return new_decl
  end
end

-- Creates a new function declaration and adds it to the current scope
--   ctx      : Compiler context
--   name     : string
--   ast_decl : ast.function_declaration
--   ir_subr  : ir.subroutine
--   -> lang.function_
local function new_function(ctx, name, ast_decl, ir_subr)
  if ctx.module_scope[name] then
    report_error('Function `'..name..'` has already been declared in this scope.')
  else
    local new_decl = lang.function_(name, ast_decl, ir_subr)
    ctx.module_scope[name] = new_decl
    return new_decl
  end
end

-- Returns the declaration corresponding to the named variable
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> lang.variable
local function find_variable(ctx, name_path)
  if #name_path > 1 then
    io.write('WARNING: Ignoring full path during search for '..name_path..'\n')
  end
  local decl = ctx.local_scope[name_path[1]]
  if decl then
    return decl
  else
    report_error('`'..name_path..'` was not found in this scope.')
  end
end

-- Returns the declaration corresponding to the named function
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> lang.function
local function find_function(ctx, name_path)
  local name_str = tostring(name_path)
  local func_decl = ctx.module_scope[name_str]
  if not func_decl then
    report_error('Function `'..name_str..'` was not found in this scope.')
  end
  return func_decl
end

----------------------------------------------------------------------------------------------------
-- IR generation

local emit
local put_call
local put_unop_expression
local put_binop_expression
local put_lvalue_expression
local put_assignment
local put_call_assignment
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
  return_regs = return_regs or { }
  -- Find the function in question
  local func_decl = find_function(ctx, name_path)
  local target_subr = func_decl.ir_subroutine
  -- Validate the function call
  if #argument_exprs ~= #func_decl.ast_function.arguments then
    report_error('Wrong number of arguments for call to '..name_path..'(...)')
  end
  if #return_regs > target_subr.size_returns then
    -- This is actually an internal error
    error('Too many return registers for function call')
  end
  -- The IR expects null registers for ignored return values. Lua can't store nil in a list, so we
  -- use '~' to denote the nil register. TODO: Put IR register names behind an API.
  for k=#return_regs+1,target_subr.size_returns do
    return_regs[k] = '~'
  end
  -- Find/generate argument expressions
  local argument_regs = { }
  for k,ast_expr in ipairs(argument_exprs) do
    argument_regs[k] = put_expression(ctx, ast_expr)
  end
  -- Actually emit call instruction
  emit(ctx, ir.call(return_regs, target_subr.name, argument_regs))
end

local function is_int64(lang_type)
  return lang_type.type == 'primitive' and lang_type.ptype == 'int64'
end

local function new_temp_int64(ctx)
  return lang.variable(nil, lang.type_int64, ctx.subroutine:alloc_register(1))
end

-- Generates IR code which computes a simple unary arithmetic expression
function put_unop_expression(ctx, expr, ir_op_fn)
  local var_x = put_expression(ctx, expr)
  if not is_int64(var_x) then
    report_error('Unary operator expects int64')
  end
  local var_z = new_temp_int64(ctx)
  emit(ctx, ir_op_fn(ir.register(var_z.allocation.first), ir.register(var_x.allocation.first)))
  return var_z
end

-- Generates IR code which compute a simple binary arithmetic expression
function put_binop_expression(ctx, expr_a, expr_b, ir_op_fn)
  local var_x = put_expression(ctx, expr_a)
  if not is_int64(var_x) then
    report_error('Unary operator expects int64')
  end
  local var_y = put_expression(ctx, expr_b)
  if not is_int64(var_y) then
    report_error('Unary operator expects int64')
  end
  local var_z = new_temp_int64(ctx)
  emit(ctx, ir_op_fn(ir.register(var_z.allocation.first),
                     ir.register(var_x.allocation.first),
                     ir.register(var_y.allocation.first)))
  return var_z
end

function compute_lvalue_eval_type(ctx, ast_lvalue)
  local var_type
  if ast_lvalue.type == 'declaration' then
    -- This lvalue expression has the base type that it declares
    var_type = find_type(ctx, ast_lvalue.type_specifier.name_path)
  elseif ast_lvalue.type == 'reference' then
    -- This lvalue expression references a variable somewhere
    var_type = find_variable(ctx, ast_lvalue.name_path).variable_type
  else
    error('')
  end
  -- Examine type
  if var_type.type == 'array' then
    if ast_lvalue.index_expressions then
      -- Normally each index expression would descend further into the type
      if #ast_lvalue.index_expressions ~= 1 then
        report_error('Multiple index expressions are unsupported.')
      end
      return var_type.value_type
    end
    -- Just an array
    return var_type
  elseif var_type.type == 'reference' then
    -- References always evaluate to their referenced type
    return var_type.value_type
  else
    -- Just your average variable
    return var_type
  end
end

function compute_expression_eval_type(ctx, expr)
  if expr.type == 'lvalue' then
    return compute_lvalue_eval_type(ctx, expr.lvalue)
  else
    -- Everything else produces an int, I think
    return lang.type_int64
  end
end

-- Evaluates a non-temporary lvalue expression to a pointer given its original variable declaration
-- and lvalue of the AST.
-- Returns the IR register object containing the pointer.
function put_lvalue_expression(ctx, var_decl, lvalue_ast)
  if not lvalue_ast.index_expressions then
    report_error('Invalid use of array `'..lvalue_ast.name_path..'` in expression')
  end
  -- Get the address, offset, and write.
  local reg_idx = put_expression(ctx, lvalue_ast.index_expressions[1])
  local reg_offs = new_temp_reg(ctx)
  local reg_ptr = new_temp_reg(ctx)
  emit(ctx, ir.stackaddr(reg_ptr, var_decl.allocation.id))
  emit(ctx, ir.mul(reg_offs, reg_idx, '8'))
  emit(ctx, ir.add(reg_ptr, reg_ptr, reg_offs))
  return reg_ptr
end

local function is_assignment_valid(ctx, lvalue_type, expr_type)
  -- For now, only ints are possible
  return lvalue_type.type == 'primitive' and lvalue_type.ptype == 'int64' and
         expr_type.type == 'primitive' and expr_type.ptype == 'int64'
end

local function put_assignment_ll_init(ctx, dst_var)
  local dst_alloc = dst_var.allocation
  -- Punt, assume always a single register
  assert(dst_alloc.type == 'register')
  assert(dst_alloc.count == 1)
  emit(ctx, ir.mov(ir.register(dst_alloc.first), ir.literal(0)))
end

local function put_assignment_ll_copy_init(ctx, dst_var, src_var)
  local dst_alloc = dst_var.allocation
  local src_alloc = src_var.allocation
  -- Punt, assume always a single register
  assert(dst_alloc.type == 'register')
  assert(dst_alloc.count == 1)
  assert(src_alloc.type == 'register')
  assert(src_alloc.count == 1)
  local dst_reg = ir.register(dst_alloc.first)
  local src_reg = ir.register(src_alloc.first)
  emit(ctx, ir.mov(dst_reg, src_reg))
end

local function put_assignment_ll_move(ctx, dst_var, src_var)
  local dst_alloc = dst_var.allocation
  local src_alloc = src_var.allocation
  assert(src_alloc.type == 'register')
  local src_reg = ir.register(src_alloc.first)
  if dst_alloc.type == 'register' then
    -- Calculate destination register and move
    local dst_reg = ir.register(dst_alloc.first)
    emit(ctx, ir.mov(dst_reg, src_reg))
  elseif dst_alloc.type == 'argument' then
    -- This is a function argument -modification is strictly forbidden.
    report_error('Attempt to assign function argument `'..
                 (lvalue_var.name_path or lvalue_var.name)..'`')
  elseif dst_alloc.type == 'stack' then
    -- Calculate destination address and store
    local dst_ptr_reg = put_lvalue_expression(ctx, lvalue_var, lvalues[i])
    emit(ctx, ir.str(dst_ptr_reg, src_reg))
  else
    assert(false)
  end
end

local function put_assignment(ctx, ast_lvalues, ast_expressions)
  -- We expect a full assignment here
  assert(#ast_lvalues == #ast_expressions)
  local n = #ast_lvalues
  -- Lvalue types, by sub-assignment index
  local lvalue_types = { }
  -- Expression types, by sub-assignment index
  local expr_types = { }
  -- Temporary value allocations, by sub-assignment index
  local temp_vars = { }
  -- Pre-process assignment ops and validate
  for i=1,n do
    -- Compute assignment operand types
    lvalue_types[i] = compute_lvalue_eval_type(ctx, ast_lvalues[i])
    expr_types[i] = compute_expression_eval_type(ctx, ast_expressions[i])
    -- Verify we can make assignment
    if not is_assignment_valid(ctx, lvalue_types[i], expr_types[i]) then
      report_error('Invalid assignment')
    end
  end
  -- Pre-evaluate the expressions for referential lvalues, copy to temporary space
  for i=1,n do
    if ast_lvalues[i].type == 'reference' then
      -- Create temporary variable to hold this expression
      temp_vars[i] = lang.variable(nil, expr_types[i], allocate_local(ctx, expr_types[i]))
      local expr_var = put_expression(ctx, ast_expressions[i])
      -- Copy-initialize to expression value
      put_assignment_ll_copy_init(ctx, temp_vars[i], expr_var)
    end
  end
  -- Move temporary expression results into referential lvalues
  -- Declare and initialize lvalues which were declared by this assignment statement
  for i=1,n do
    if ast_lvalues[i].type == 'reference' then
      assert(temp_vars[i])
      -- Find destination
      local lvalue_var = find_variable(ctx, ast_lvalues[i].name_path)
      -- Move stored expression value
      put_assignment_ll_move(ctx, lvalue_var, temp_vars[i])
    elseif ast_lvalues[i].type == 'declaration' then
      -- Declarations can't be referenced on the right hand side, so their expression result doesn't
      -- need to be evaluated and saved beforehand.
      -- Declare this variable
      local lvalue_var = new_local_variable(ctx, ast_lvalues[i].name, lvalue_types[i])
      -- Evaluate expression
      local expr_var = put_expression(ctx, ast_expressions[i])
      -- Copy-initialize
      put_assignment_ll_copy_init(ctx, lvalue_var, expr_var)
    end
  end
end

local function put_null_assignment(ctx, ast_lvalues)
  for i,ast_lvalue in ipairs(ast_lvalues) do
    if ast_lvalue.type == 'declaration' then
      -- Declare this variable
      local type_ = compute_type(ctx, ast_lvalue.type_specifier)
      local var = new_local_variable(ctx, ast_lvalue.name, type_)
      -- Default-initialize
      put_assignment_ll_init(ctx, var)
    else
      report_error('Invalid use of non-declarational lvalue in default initialization')
    end
  end
end

-- Generates IR code which calls a function and stores its return values in the given lvalues.
function put_call_assignment(ctx, lvalue_exprs, call_expr)
  assert(call_expr.type == 'call')
  -- Validate function call
  local function_path = call_expr.name_path
  local target_subr = find_function(ctx, function_path).ir_subroutine
  if target_subr.size_returns < #lvalue_exprs then
    report_error('Cannot assign '..plz(target_subr.size_returns, 'return value', 'return values')..
                 ' to '..plz(#lvalue_exprs, 'lvalue', 'lvalues')..'.')
  end
  -- Allocate some registers for this function to shit into.
  local lvalue_decls = { }
  local return_regs = { }
  for i=1,#lvalue_exprs do
    local decl = find_or_declare_lvalue(ctx, lvalue_exprs[i])
    lvalue_decls[i] = decl
    -- Allocate new registers depending on allocation.
    if decl.allocation.type == 'register' then
      -- This lvalue has temporary allocation, so it's fair game to assign directly.
      return_regs[i] = ir.register(decl.allocation.first)
    elseif decl.allocation.type == 'stack' then
      -- We'll have to store this value somewhere else, so allocate it a temporary.
      return_regs[i] = new_temp_reg(ctx)
    elseif decl.allocation.type == 'argument' then
      report_error('Attempt to assign function argument `'..name..'`')
    end
  end
  -- Shit into return_regs.
  put_call(ctx, function_path, call_expr.arguments, return_regs)
  -- Assign registers where they actually must go.
  for i=1,#lvalue_exprs do
    local decl = lvalue_decls[i]
    if decl.allocation.type == 'stack' then
      -- We created a temporary for this return value before the call.
      -- Compute the destination pointer and store!
      local reg_ptr = put_lvalue_expression(ctx, decl, lvalue_exprs[i])
      emit(ctx, ir.str(reg_ptr, return_regs[i]))
    end
  end
end

-- TODO: Needs to start returning an IR allocation and a type, not an IR register
-- Generates IR code which evaluates the given AST expression
-- Returns an IR operand (not necessarily a new temporary) which contains the expression result
function put_expression(ctx, expr)
  local h = pet[expr.type]
  if h then
    return h(ctx, expr)
  else
    error('Unknown expression type `'..expr.type..'`\n')
  end
end

function pet.integer(ctx, expr)
  local var_z = new_temp_int64(ctx)
  emit(ctx, ir.mov(ir.register(var_z.allocation.first), ir.literal(tonumber(expr.value))))
  return var_z
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
  local lab1 = new_label(ctx)
  local lab2 = new_label(ctx)
  local var_z = new_temp_int64(ctx)
  local reg_z = ir.register(var_z.allocation.first)
  local var_x = put_expression(ctx, expr.expression_a)
  if not is_int64(var_x) then
    report_error('Logical operator expects int64')
  end
  local reg_x = ir.register(var_x.allocation.first)
  -- If nonzero, jump to the evaluation of the next expression
  -- Otherwise, set reg_z to zero, and jump to end
  emit(ctx, ir.jnz(lab1, reg_x));
  emit(ctx, ir.mov(reg_z, 0));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  local var_y = put_expression(ctx, expr.expression_b)
  if not is_int64(var_y) then
    report_error('Logical operator expects int64')
  end
  local reg_y = ir.register(var_y.allocation.first)
  -- Set reg_z to zero iff reg_y is zero
  emit(ctx, ir.neq(reg_z, reg_y, ir.literal(0)))
  emit(ctx, ir.label(lab2))
  -- Our result is in var_z
  return var_z
end

function pet.logor(ctx, expr)
  local lab1 = new_label(ctx)
  local lab2 = new_label(ctx)
  -- Create a new temporary for result
  local var_z = new_temp_int64(ctx)
  local reg_z = ir.register(var_z.allocation.first)
  -- Put first expression, validate type
  local var_x = put_expression(ctx, expr.expression_a)
  if not is_int64(var_x) then
    report_error('Logical operator expects int64')
  end
  local reg_x = ir.register(var_x.allocation.first)
  -- If reg_x is zero, jump to the evaluation of the next expression
  -- Otherwise, set reg_z to one, and jump to end
  emit(ctx, ir.jz(lab1, reg_x));
  emit(ctx, ir.mov(reg_z, 1));
  emit(ctx, ir.jmp(lab2));
  emit(ctx, ir.label(lab1))
  -- Put second expression, validate type
  local var_y = put_expression(ctx, expr.expression_b)
  if not is_int64(var_y) then
    report_error('Logical operator expects int64')
  end
  local reg_y = ir.register(var_y.allocation.first)
  -- Set reg_z to zero iff reg_y is zero
  emit(ctx, ir.neq(reg_z, reg_y, ir.literal(0)))
  emit(ctx, ir.label(lab2))
  -- Our result is in var_z
  return var_z
end

function pet.lvalue(ctx, expr)
  -- This is an lvalue expression, so it must have a declaration
  local base_var = find_variable(ctx, expr.lvalue.name_path)
  if base_var.allocation.type == 'register' then
    return base_var
  elseif base_var.allocation.type == 'argument' then
    return base_var
  elseif base_var.allocation.type == 'stack' then
    -- Create temporary variable to hold result of this expression
    local expr_type = compute_lvalue_eval_type(ctx, expr.lvalue)
    if not is_int64(expr_type) then
      report_error('Lvalue expression must evaluate to int64')
    end
    local temp_var = lang.variable(nil, expr_type, allocate_local(ctx, expr_type))
    -- Calculate lvalue address and load into temp space
    local reg_ptr = put_lvalue_expression(ctx, base_var, expr.lvalue)
    emit(ctx, ir.ldr(ir.register(temp_var.allocation.first), reg_ptr))
    -- Result is in new temporary
    return temp_var
  else
    error('')
  end
end

function pet.call(ctx, expr)
  -- Function calls only yield their first return value to expressions
  local dst_var = new_temp_int64(ctx)
  put_call(ctx, expr.name_path, expr.arguments, { dst_var })
  return dst_var
end

-- Generates IR code which implements an AST statement.
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
    local lab1 = new_label(ctx)
    local lab2 = new_label(ctx)
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
    local lab1 = new_label(ctx)
    -- Put conditional expression into new temporary
    local expr_reg = put_expression(ctx, ast_stmt.expression)
    emit(ctx, ir.jz(lab1, expr_reg))
    -- Put if statement body
    put_statement(ctx, ast_stmt.if_statement)
    emit(ctx, ir.label(lab1))
  end
end

pst['return'] = function(ctx, ast_stmt)
  if #ctx.ast_function.returns ~= #ast_stmt.expressions then
    report_error('Function returns '..plz(#ctx.ast_function.returns, 'value', 'values')..
                 ', but '..plz(#ast_stmt.expressions, 'was', 'were')..
                 ' provided')
  end
  -- Prepare for multiple assignment
  local n = #ctx.ast_function.returns
  for i=1,n do
    io.write('PUNTING ON RETURN\n')
    --[[
    local tmp_var = put_expression(ctx, ast_stmt.expressions[i])
    io.write('reg:\n')
    pprint(reg)
    emit(ctx, ir.mov(ir.retreg(i-1), reg))
    ]]
  end
  emit(ctx, ir.ret())
end

function pst.assignment(ctx, ast_stmt)
  -- Validate general assignment configuration
  local num_expressions = #ast_stmt.expressions
  local num_lvalues = #ast_stmt.lvalues
  if num_lvalues == num_expressions then
    put_assignment(ctx, ast_stmt.lvalues, ast_stmt.expressions)
  elseif num_expressions == 0 then
    put_null_assignment(ctx, ast_stmt.lvalues)
  else
    local first_expr = ast_stmt.expressions[1]
    -- If the expression list is a single call, expand its return values to the given lvalues.
    if num_expressions == 1 and first_expr.type == 'call' then
      put_call_assignment(ctx, ast_stmt.lvalues, first_expr)
    else
      report_error('Invalid assignment statement.')
    end
  end
end

function pst.block(ctx, ast_stmt)
  -- Save current scope
  local local_scope_prev = ctx.local_scope
  ctx.local_scope = copy_scope(ctx.local_scope)
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

local function put_function(ctx, ast_func)
  -- Create a new IR subroutine for this function (we will continue to build it)
  local ir_subr = ctx.program:add_subroutine('z$'..ast_func.name)
  -- Declare function in the current module scope
  local func_decl = new_function(ctx, ast_func.name, ast_func, ir_subr)
  -- Allocate argument registers and populate initial local scope
  local local_scope = { }
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local name = ast_arg_decl.name
    -- Compute the type from the AST type specifier
    local arg_type = compute_type(ctx, ast_arg_decl.type_specifier)
    if not (arg_type.type == 'primitive' and arg_type.ptype == 'int64') then
      report_error('Argument value types other than int64 are unsupported.')
    end
    -- Allocate this argument
    local size_quadwords = 1
    local allocation = ir_subr:alloc_argument(size_quadwords)
    -- Place in local scope
    local_scope[name] = lang.variable(name, arg_type, allocation)
  end
  -- Allocate return registers
  for i,ast_arg_decl in ipairs(ast_func.returns) do
    local size_quadwords = 1
    ir_subr:alloc_return(size_quadwords)
  end
  -- Initialize subroutine processing context
  ctx.subroutine = ir_subr
  ctx.ast_function = ast_func
  -- Counter for unique label generation
  ctx.label_index = 0
  -- Location of next stack allocation
  ctx.stack_index = 0
  -- Map from local variable names to declarations
  ctx.local_scope = local_scope
  -- Read statements from AST
  pst.block(ctx, ast_func.block)
end

local function compile(ast)
  local ctx = { }
  -- Map from absolute function names to declarations
  ctx.module_scope = { }
  -- Final object representing all compiled code
  ctx.program = ir.program()
  -- Find each function and compile it into a subroutine
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

