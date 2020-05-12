
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
local put_expression_h = { }
-- (p)ut (s)tatement (t)able
local put_statement_h = { }

-- Appends an instruction onto the subroutine currently being generated
function emit(ctx, ir_stmt)
  local stmts = ctx.subroutine.statements
  stmts[#stmts+1] = ir_stmt
end

-- TODO: Needs to start returning an IR allocation and a type, not an IR register
-- Generates IR code which evaluates the given AST expression
-- Returns an IR operand (not necessarily a new temporary) which contains the expression result
function put_expression(ctx, expr)
  local h = put_expression_h[expr.type]
  if h then
    return h(ctx, expr)
  else
    error('Unknown expression type `'..expr.type..'`\n')
  end
end

function put_expression_h.integer(ctx, expr)
end

function put_expression_h.negate(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.neg)
end

function put_expression_h.binnot(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.bnot)
end

function put_expression_h.lognot(ctx, expr)
  return put_unop_expression(ctx, expr.expression, ir.lnot)
end

function put_expression_h.add(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.add)
end

function put_expression_h.sub(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.sub)
end

function put_expression_h.mul(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.mul)
end

function put_expression_h.div(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.div)
end

function put_expression_h.cmpeq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.eq)
end

function put_expression_h.cmpneq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.neq)
end

function put_expression_h.cmplt(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.lt)
end

function put_expression_h.cmpgt(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.gt)
end

function put_expression_h.cmpleq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.leq)
end

function put_expression_h.cmpgeq(ctx, expr)
  return put_binop_expression(ctx, expr.expression_a, expr.expression_b, ir.geq)
end

function put_expression_h.logand(ctx, expr)
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

function put_expression_h.logor(ctx, expr)
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

function put_expression_h.lvalue(ctx, expr)
end

function put_expression_h.call(ctx, expr)
end

-- Generates IR code which implements an AST statement.
function put_statement(ctx, ast_stmt)
  local h = put_statement_h[ast_stmt.type]
  if h then
    h(ctx, ast_stmt)
  else
    report_error('Unknown AST statement type `'..ast_stmt.type..'`')
  end
end

put_statement_h['if'] = function(ctx, ast_stmt)
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

put_statement_h['return'] = function(ctx, ast_stmt)
end

function put_statement_h.assignment(ctx, ast_stmt)
end

function put_statement_h.block(ctx, ast_stmt)
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

function put_statement_h.call(ctx, ast_stmt)
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
  put_statement_h.block(ctx, ast_func.block)
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

