
----------------------------------------------------------------------------------------------------
-- Compile AST into IR
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local pprint = require 'pprint'

----------------------------------------------------------------------------------------------------
-- Language declarations

local decl = { }

function decl.local_variable(name, type_spec, ir_alloc)
  assert(type(name) == 'string')
  assert(type(type_spec) == 'table')
  assert(type(ir_alloc) == 'table')
  return { type = 'local', type_specifier = type_spec, name = name, ir_alloc = ir_alloc }
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
local function new_temp_index(ctx)
  local idx = ctx.temp_index
  ctx.temp_index = ctx.temp_index + 1
  return idx
end

-- Stack storage is allocated via simple stacking
local function alloc_stack_space(ctx, size)
  local idx = ctx.stack_index
  ctx.stack_index = ctx.stack_index + size
  return idx, size
end

local function new_temp_reg(ctx)
  local ir_alloc = ctx.subroutine:alloc_temporary(1)
  return ir.tempreg(ir_alloc.first)
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
  local decl = ctx.local_scope[name]
  if decl then
    return decl
  else
    report_error('`'..name..'` was not found in this scope.')
  end
end

-- Returns the subroutine corresponding to the given function path
-- TODO: Allowing module members to be referenced by absolute path necessitates merging this search
-- with the search for local variables!
-- TODO: Return declaration information, not the IR subroutine
local function find_call(ctx, name_path)
  local name = table.concat(name_path, '$')
  local subr = ctx.subroutines[name]
  if not subr then
    report_error('Function '..table.concat(name_path, ':')..' is undefined here.')
  end
  return subr
end

local function type_is_atomic(type_spec)
end

-- Attempts to create a new local variable in the current scope
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
    local new_decl = decl.local_variable(name, type_spec, ir_alloc)
    ctx.local_scope[name] = new_decl
    return new_decl
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
  if #argument_exprs ~= target_subr.size_arguments then
    report_error('Wrong number of arguments for call to '..table.concat(name_path, ':')..'(...)')
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

-- Evaluates a non-temporary lvalue expression to a pointer
-- Returns the register containing the pointer
local function put_lvalue_expression(ctx, lvalue_decl, ast_lvalue)
  -- This is some other type, present specifically on the stack
  if not lvalue_decl.type_specifier.quantity then
    local name = table.concat(ast_lvalue.name_path,':')
    error('We have a stack-allocated variable `'..name..'` that isn\'t an array?')
  end
  -- This is an array on the stack: take the address, offset, and write.
  local reg_idx = put_expression(ctx, ast_lvalue.index_expressions[1])
  local reg_ptr = new_temp_reg(ctx)
  emit(ctx, ir.stackaddr(reg_ptr, lvalue_decl.ir_alloc.offset))
  emit(ctx, ir.add(reg_ptr, reg_ptr, reg_idx))
  return reg_ptr
end

local function init_lvalue_temporary(ctx, lvalue_decl, ast_lvalue, ast_expr)
  local name = ast_lvalue.name
  if ast_expr then
    -- Initialize to expression
    emit(ctx, ir.mov(ir.tempreg(lvalue_decl.ir_alloc.first), put_expression(ctx, ast_expr)))
  else
    -- Zero-initialize by default
    emit(ctx, ir.mov(ir.tempreg(lvalue_decl.ir_alloc.first), '0'))
  end
end

local function init_lvalue_stack(ctx, lvalue_decl, ast_lvalue, ast_expr)
  local name = ast_lvalue.name
  io.write('WARNING: Skipping initialization of stack-allocated variable `'..name..'`\n')
end

local function assign_lvalue_temporary(ctx, lvalue_decl, ast_lvalue, ast_expr)
  -- Assignment to temporary storage means generating simple mov instructions
  assert(lvalue_decl.ir_alloc.count == 1,
         'Register assignments are (currently) unsupported for count != 1')
  emit(ctx, ir.mov(ir.tempreg(lvalue_decl.ir_alloc.first), put_expression(ctx, ast_expr)))
end

local function assign_lvalue_stack(ctx, lvalue_decl, ast_lvalue, ast_expr)
  local reg_val = put_expression(ctx, ast_expr)
  local reg_ptr = put_lvalue_expression(ctx, lvalue_decl, ast_lvalue)
  emit(ctx, ir.str(reg_ptr, reg_val))
end

local function assign_lvalue(ctx, ast_lvalue, ast_expr)
  local decl
  if ast_lvalue.type == 'declaration' then
    -- Declare a new variable
    local name = ast_lvalue.name
    decl = new_local_variable(ctx, ast_lvalue.name, ast_lvalue.type_specifier)
    -- Initialize this variable
    if decl.ir_alloc.type == 'temporary' then
      init_lvalue_temporary(ctx, decl, ast_lvalue, ast_expr)
    elseif decl.ir_alloc.type == 'stack' then
      init_lvalue_stack(ctx, decl, ast_lvalue, ast_expr)
    else
      error('Cannot initialize `'..name..'`, of storage type `'..decl.ir_alloc.type..'`')
    end
  elseif ast_lvalue.type == 'reference' then
    local name = table.concat(ast_lvalue.name_path,':')
    -- There is no "default" assignment for a referential lvalue.
    if ast_expr == nil then
      report_error('Missing rvalue for assignment to `'..name..'`')
    end
    -- Find existing variable
    if #ast_lvalue.name_path ~= 1 then
      report_error('Fully-scoped referential lvalues not supported')
    end
    decl = find_variable(ctx, ast_lvalue.name_path[1])
    -- Assign to this variable
    if decl.ir_alloc.type == 'temporary' then
      assign_lvalue_temporary(ctx, decl, ast_lvalue, ast_expr)
    elseif decl.ir_alloc.type == 'argument' then
      -- This is a function argument -modification is strictly forbidden.
      report_error('Attempt to assign function argument `'..name..'`')
    elseif decl.ir_alloc.type == 'stack' then
      assign_lvalue_stack(ctx, decl, ast_lvalue, ast_expr)
    else
      error('Cannot initialize `'..name..'`, of storage type `'..decl.ir_alloc.type..'`')
    end
  else
    -- This is an impossibility!
    error('Invalid lvalue type `'..tostring(ast_lvalue.type)..'`')
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
  local decl = find_variable(ctx, expr.name)
  if decl.ir_alloc.type == 'temporary' then
    assert(decl.ir_alloc.count == 1)
    return ir.tempreg(decl.ir_alloc.first)
  elseif decl.ir_alloc.type == 'argument' then
    assert(decl.ir_alloc.count == 1)
    return ir.argreg(decl.ir_alloc.first)
  elseif decl.ir_alloc.type == 'stack' then
    if not decl.type_specifier.quantity then
      error('We have a stack-allocated variable `'..expr.name..'` that isn\'t an array?')
    end
    -- This is an array type, expect index expressions
    if not expr.index_expressions then
      report_error('Invalid use of array `'..expr.name..'` in expression')
    end
    -- Calculate address and load
    local reg_idx = put_expression(ctx, expr.index_expressions[1])
    local reg_ptr = new_temp_reg(ctx)
    local reg_val = new_temp_reg(ctx)
    emit(ctx, ir.stackaddr(reg_ptr, decl.ir_alloc.offset))
    emit(ctx, ir.add(reg_ptr, reg_ptr, reg_idx))
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

-- Emits IR code which implements an assignment statement
function pst.assignment(ctx, ast_stmt)
  -- Validate assignment configuration
  local num_expressions = #ast_stmt.expressions
  local num_lvalues = #ast_stmt.lvalues
  if num_lvalues == num_expressions then
    for i=1,num_lvalues do
      assign_lvalue(ctx, ast_stmt.lvalues[i], ast_stmt.expressions[i])
    end
  elseif num_expressions == 0 then
    -- Default initialize declarations
    -- If an lvalue isn't a declaration, this will fail accordingly
    for i=1,num_lvalues do
      assign_lvalue(ctx, ast_stmt.lvalues[i], nil)
    end
  else
    local first_expr = ast_stmt.expressions[1]
    -- If the expression list is a single call, expand its return values to the given lvalues
    if num_expressions == 1 and first_expr.type == 'call' then
      local ir_subr = find_call(ctx, first_expr.name_path)
      if ir_subr.size_returns < num_lvalues then
        report_error('Cannot assign '..plz(ir_subr.size_returns, 'return value', 'return values')..
                     ' to '..plz(num_lvalues, 'lvalue', 'lvalues')..'.')
      end
      -- Allocate some registers for this function to shit into
      local lvalue_decls = { }
      local lvalue_regs = { }
      for i=1,num_lvalues do
        -- Find the declaration which governs this lvalue
        local lvalue_decl
        local ast_lvalue = ast_stmt.lvalues[i]
        if ast_lvalue.type == 'declaration' then
          lvalue_decl = new_local_variable(ctx, ast_lvalue.name, ast_lvalue.type_specifier)
        elseif ast_lvalue.type == 'reference' then
          lvalue_decl = find_variable(ctx, ast_lvalue.name_path[1])
        end
        lvalue_decls[i] = lvalue_decl
        -- Allocate new registers depending on allocation
        if lvalue_decl.ir_alloc.type == 'temporary' then
          -- This lvalue has temporary allocation, so it's fair game to assign directly
          lvalue_regs[i] = ir.tempreg(lvalue_decl.ir_alloc.first)
        elseif lvalue_decl.ir_alloc.type == 'stack' then
          -- TODO: Validate type of lvalue expression result
          lvalue_regs[i] = new_temp_reg(ctx)
        elseif lvalue_decl.ir_alloc.type == 'argument' then
          report_error('Attempt to assign function argument `'..name..'`')
        end
      end
      -- Shit into lvalue_regs
      put_call(ctx, first_expr.name_path, first_expr.arguments, lvalue_regs)
      -- Assign registers where they actually must go
      for i=1,num_lvalues do
        lvalue_decl = lvalue_decls[i]
        if lvalue_decl.ir_alloc.type == 'stack' then
          local ast_lvalue = ast_stmt.lvalues[i]
          local reg_ptr = put_lvalue_expression(ctx, lvalue_decl, ast_lvalue)
          emit(ctx, ir.str(reg_ptr, lvalue_regs[i]))
        end
      end
    else
      report_error('Invalid assignment statement.')
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
  local subr = ir.subroutine()
  -- Populate initial scope with argument registers
  local local_scope = { }
  for i,ast_arg_decl in ipairs(ast_func.arguments) do
    local name = ast_arg_decl.name
    local type_spec = ast_arg_decl.type_specifier
    local size_quadwords = 1
    local ir_alloc = subr:alloc_argument(1)
    local_scope[name] = decl.local_variable(name, type_spec, ir_alloc)
  end
  for i,ast_arg_decl in ipairs(ast_func.returns) do
    local size_quadwords = 1
    subr:alloc_return(1)
  end
  -- Initialize subroutine context
  ctx.subroutine = subr
  ctx.ast_function = ast_func
  -- Counter / high water mark for temporary allocation
  ctx.temp_index = 0
  -- Counter / high water mark for stack allocation
  ctx.stack_index = 0
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

