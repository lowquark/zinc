
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local pprint = require 'pprint'

----------------------------------------------------------------------------------------------------
-- Language declarations

local decl = { }

function decl.variable(name, type_spec, ir_alloc)
  assert(type(name) == 'string')
  assert(type(type_spec) == 'table')
  assert(type(ir_alloc) == 'table')
  return { type = 'local', type_specifier = type_spec, name = name, ir_alloc = ir_alloc }
end

-- `name_path` must be absolute.
function decl.function_(name, ast_func, ir_subr)
  assert(type(name) == 'string')
  assert(type(ast_func) == 'table')
  assert(type(ir_subr) == 'table')
  return { name = name,
           ast_function = ast_func,
           ir_subroutine = ir_subr }
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

-- Returns a subroutine-unique label
local function new_label(ctx)
  local idx = ctx.label_index
  ctx.label_index = ctx.label_index + 1
  return 'l'..idx
end

-- Stack storage is allocated via simple stacking
local function alloc_stack_space(ctx, size)
  local idx = ctx.stack_index
  ctx.stack_index = ctx.stack_index + size
  return idx, size
end

-- Conveniently allocates a single temporary register
local function new_temp_reg(ctx)
  local ir_alloc = ctx.subroutine:alloc_temporary(1)
  return ir.tempreg(ir_alloc.first)
end

-- Creates a new function declaration and adds it to the current scope
--   ctx      : Compiler context
--   name     : string
--   ast_decl : ast.function_declaration
--   ir_subr  : ir.subroutine
--   -> decl.function
local function new_function(ctx, name, ast_decl, ir_subr)
  if ctx.module_scope[name] then
    report_error('Function `'..name..'` has already been declared in this scope.')
  else
    local new_decl = decl.function_(name, ast_decl, ir_subr)
    ctx.module_scope[name] = new_decl
    return new_decl
  end
end

-- Attempts to create a new local variable in the current scope
--   ctx       : Compiler context
--   name      : string
--   type_spec : ast.type_specifier
--   -> decl.variable
local function new_local_variable(ctx, name, type_spec)
  if ctx.local_scope[name] then
    report_error('`'..name..'` was already declared in this scope.')
  else
    local ir_alloc
    if type_spec.quantity then
      local size_quadwords = type_spec.quantity
      local offset, size = alloc_stack_space(ctx, size_quadwords)
      -- Allocate a new block of stack space to this variable
      ir_alloc = ctx.subroutine:alloc_stack(offset, size)
      -- Pretty print
      local first = tostring(ir_alloc.offset)
      local last = tostring(ir_alloc.offset + ir_alloc.size - 1)
      io.write('local `'..name..'` is stack allocated on ['..first..', '..last..']\n')
    else
      -- Allocate a new temporary register to this variable
      local size_quadwords = 1
      ir_alloc = ctx.subroutine:alloc_temporary(size_quadwords)
      -- Pretty print
      local first = 'r'..ir_alloc.first
      local last = 'r'..(ir_alloc.first + ir_alloc.count - 1)
      io.write('local `'..name..'` is register allocated on ['..first..', '..last..']\n')
    end
    -- Associate this declaration with this variable name in the current scope
    local new_decl = decl.variable(name, type_spec, ir_alloc)
    ctx.local_scope[name] = new_decl
    return new_decl
  end
end

-- Returns the declaration corresponding to the named function
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> decl.function
local function find_function(ctx, name_path)
  local name_str = tostring(name_path)
  local func_decl = ctx.module_scope[name_str]
  if not func_decl then
    report_error('Function `'..name_str..'` was not found in this scope.')
  end
  return func_decl
end

-- Returns the declaration corresponding to the named variable
--   ctx       : Compiler context
--   name_path : ast.name_path
--   -> decl.variable
local function find_variable(ctx, name_path)
  if #name_path > 1 then
    io.write('WARNING: Ignoring full path during search for '..name_path..'\n')
  end
  local decl = ctx.local_scope[name_path[1]]
  if decl then
    return decl
  else
    report_error('`'..name..'` was not found in this scope.')
  end
end

-- If the lvalue is a declaration type, creates a new local and returns the declaration object
-- If the lvalue is a reference type, finds and returns the declaration object
--   ctx        : Compiler context
--   ast_lvalue : ast.lvalue
--   -> decl.variable
local function get_lvalue_declaration(ctx, ast_lvalue)
  if ast_lvalue.type == 'declaration' then
    return new_local_variable(ctx, ast_lvalue.name, ast_lvalue.type_specifier)
  elseif ast_lvalue.type == 'reference' then
    return find_variable(ctx, ast_lvalue.name_path)
  else
    error()
  end
end

----------------------------------------------------------------------------------------------------
-- IR generation

local emit
local put_call
local put_unop_expression
local put_binop_expression
local put_lvalue_expression
local init_lvalue_temporary
local init_lvalue_stack
local assign_lvalue_temporary
local assign_lvalue_stack
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

-- Generates IR code which computes a simple unary arithmetic expression
function put_unop_expression(ctx, expr, ir_op_fn)
  local reg_x = put_expression(ctx, expr)
  local reg_z = new_temp_reg(ctx)
  emit(ctx, ir_op_fn(reg_z, reg_x))
  return reg_z
end

-- Generates IR code which compute a simple binary arithmetic expression
function put_binop_expression(ctx, expr_a, expr_b, ir_op_fn)
  local reg_x = put_expression(ctx, expr_a)
  local reg_y = put_expression(ctx, expr_b)
  local reg_z = new_temp_reg(ctx)
  emit(ctx, ir_op_fn(reg_z, reg_x, reg_y))
  return reg_z
end

-- Evaluates a non-temporary lvalue expression to a pointer given its original variable declaration
-- and lvalue of the AST.
-- Returns the IR register object containing the pointer.
function put_lvalue_expression(ctx, var_decl, lvalue_ast)
  -- For now we're expecting only arrays here. Structs will come later.
  if not var_decl.type_specifier.quantity then
    error('We have a stack-allocated variable `'..lvalue_ast.name_path..'` that isn\'t an array?')
  end
  if not lvalue_ast.index_expressions then
    report_error('Invalid use of array `'..lvalue_ast.name_path..'` in expression')
  end
  -- Get the address, offset, and write.
  local reg_idx = put_expression(ctx, lvalue_ast.index_expressions[1])
  local reg_offs = new_temp_reg(ctx)
  local reg_ptr = new_temp_reg(ctx)
  emit(ctx, ir.stackaddr(reg_ptr, var_decl.ir_alloc.id))
  emit(ctx, ir.mul(reg_offs, reg_idx, '8'))
  emit(ctx, ir.add(reg_ptr, reg_ptr, reg_offs))
  return reg_ptr
end

-- Initializes a temporary-allocated lvalue to the given expression.
-- If `rvalue_ast` is nil, default-initializes lvalue instead.
function init_lvalue_temporary(ctx, var_decl, lvalue_ast, rvalue_ast)
  local name = lvalue_ast.name
  if rvalue_ast then
    -- Initialize to expression
    emit(ctx, ir.mov(ir.tempreg(var_decl.ir_alloc.first), put_expression(ctx, rvalue_ast)))
  else
    -- Zero-initialize by default
    emit(ctx, ir.mov(ir.tempreg(var_decl.ir_alloc.first), '0'))
  end
end

-- Initializes a stack-allocated lvalue to the given expression.
-- If `rvalue_ast` is nil, default-initializes lvalue instead.
function init_lvalue_stack(ctx, var_decl, lvalue_ast, rvalue_ast)
  io.write('WARNING: Skipping initialization of stack-allocated variable `'..var_decl.name..'`\n')
end

-- Assigns a temporary-allocated lvalue to the given expression.
function assign_lvalue_temporary(ctx, var_decl, lvalue_ast, rvalue_ast)
  if var_decl.ir_alloc.count ~= 1 then
    error('Register assignments are (currently) unsupported for count != 1')
  end
  -- Assignment to temporary storage means generating simple mov instructions
  emit(ctx, ir.mov(ir.tempreg(var_decl.ir_alloc.first), put_expression(ctx, rvalue_ast)))
end

-- Assigns a stack-allocated lvalue to the given expression.
function assign_lvalue_stack(ctx, var_decl, lvalue_ast, rvalue_ast)
  local reg_val = put_expression(ctx, rvalue_ast)
  local reg_ptr = put_lvalue_expression(ctx, var_decl, lvalue_ast)
  emit(ctx, ir.str(reg_ptr, reg_val))
end

-- Deprecated
-- Generates IR code which directly implements the given (single) assignment.
function put_assignment(ctx, lvalue_ast, rvalue_ast)
  local decl = get_lvalue_declaration(ctx, lvalue_ast)
  if lvalue_ast.type == 'declaration' then
    -- Initialize this variable
    if decl.ir_alloc.type == 'temporary' then
      init_lvalue_temporary(ctx, decl, lvalue_ast, rvalue_ast)
    elseif decl.ir_alloc.type == 'stack' then
      init_lvalue_stack(ctx, decl, lvalue_ast, rvalue_ast)
    else
      assert(false)
    end
  elseif lvalue_ast.type == 'reference' then
    -- Make sure we're not "initializing" an existing local
    if rvalue_ast == nil then
      report_error('Missing rvalue for assignment to `'..lvalue_ast.name_path..'`')
    end
    -- Assign to this variable
    if decl.ir_alloc.type == 'temporary' then
      assign_lvalue_temporary(ctx, decl, lvalue_ast, rvalue_ast)
    elseif decl.ir_alloc.type == 'argument' then
      -- This is a function argument -modification is strictly forbidden.
      report_error('Attempt to assign function argument `'..lvalue_ast.name_path..'`')
    elseif decl.ir_alloc.type == 'stack' then
      assign_lvalue_stack(ctx, decl, lvalue_ast, rvalue_ast)
    else
      assert(false)
    end
  else
    -- This is an impossibility!
    error('Invalid lvalue type `'..tostring(lvalue_ast.type)..'`')
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
    local decl = get_lvalue_declaration(ctx, lvalue_exprs[i])
    lvalue_decls[i] = decl
    -- Allocate new registers depending on allocation.
    if decl.ir_alloc.type == 'temporary' then
      -- This lvalue has temporary allocation, so it's fair game to assign directly.
      return_regs[i] = ir.tempreg(decl.ir_alloc.first)
    elseif decl.ir_alloc.type == 'stack' then
      -- We'll have to store this value somewhere else, so allocate it a temporary.
      return_regs[i] = new_temp_reg(ctx)
    elseif decl.ir_alloc.type == 'argument' then
      report_error('Attempt to assign function argument `'..name..'`')
    end
  end
  -- Shit into return_regs.
  put_call(ctx, function_path, call_expr.arguments, return_regs)
  -- Assign registers where they actually must go.
  for i=1,#lvalue_exprs do
    local decl = lvalue_decls[i]
    if decl.ir_alloc.type == 'stack' then
      -- We created a temporary for this return value before the call.
      -- Compute the destination pointer and store!
      local reg_ptr = put_lvalue_expression(ctx, decl, lvalue_exprs[i])
      emit(ctx, ir.str(reg_ptr, return_regs[i]))
    end
  end
end

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
  local lab1 = new_label(ctx)
  local lab2 = new_label(ctx)
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
  local lab1 = new_label(ctx)
  local lab2 = new_label(ctx)
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

function pet.lvalue(ctx, expr)
  local decl = find_variable(ctx, expr.lvalue.name_path)
  if decl.ir_alloc.type == 'temporary' then
    assert(decl.ir_alloc.count == 1)
    return ir.tempreg(decl.ir_alloc.first)
  elseif decl.ir_alloc.type == 'argument' then
    assert(decl.ir_alloc.count == 1)
    return ir.argreg(decl.ir_alloc.first)
  elseif decl.ir_alloc.type == 'stack' then
    -- Calculate address and store!
    local reg_ptr = put_lvalue_expression(ctx, decl, expr.lvalue)
    local reg_val = new_temp_reg(ctx)
    emit(ctx, ir.ldr(reg_val, reg_ptr))
    -- Result is in new temporary
    return reg_val
  else
    error('Invalid declaration IR allocation type `'..decl.ir_alloc.type..'`')
  end
end

function pet.call(ctx, expr)
  -- Function calls only yield their first return value to expressions
  local dst_reg = new_temp_reg(ctx)
  put_call(ctx, expr.name_path, expr.arguments, { dst_reg })
  return dst_reg
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
    emit(ctx, ir.mov(ir.retreg(i-1), put_expression(ctx, ast_stmt.expressions[i])))
  end
  emit(ctx, ir.ret())
end

function pst.assignment(ctx, ast_stmt)
  -- Validate general assignment configuration.
  local num_expressions = #ast_stmt.expressions
  local num_lvalues = #ast_stmt.lvalues
  if num_lvalues == num_expressions then
    for i=1,num_lvalues do
      put_assignment(ctx, ast_stmt.lvalues[i], ast_stmt.expressions[i])
    end
  elseif num_expressions == 0 then
    -- Default initialize declarations
    -- If an lvalue isn't a declaration, this will fail accordingly.
    for i=1,num_lvalues do
      put_assignment(ctx, ast_stmt.lvalues[i], nil)
    end
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
    local type_spec = ast_arg_decl.type_specifier
    local size_quadwords = 1
    local ir_alloc = ir_subr:alloc_argument(size_quadwords)
    local_scope[name] = decl.variable(name, type_spec, ir_alloc)
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

