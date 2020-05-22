
----------------------------------------------------------------------------------------------------
-- cc - (c)ompiler (c)onstructions
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local pprint = require 'pprint'

local cc = { }

----------------------------------------------------------------------------------------------------
-- All types declared
--

local hard_type_methods = { }
local hard_type_meta = { __index = hard_type_methods }

local variable_type_methods = { }
local variable_type_meta = { __index = variable_type_methods }

local variable_methods = { }
local variable_meta = { __index = variable_methods }

local function_methods = { }
local function_meta = { __index = function_methods }

local expression_proxy_methods = { }
local expression_proxy_meta = { __index = expression_proxy_methods }

local lvalue_proxy_methods = { }
local lvalue_proxy_meta = { __index = lvalue_proxy_methods }

local block_stack_methods = { }
local block_stack_meta = { __index = block_stack_methods }

local module_stack_methods = { }
local module_stack_meta = { __index = module_stack_methods }

----------------------------------------------------------------------------------------------------
-- cc.hard_type
--
-- Defines pure data, no references, no const

function hard_type_meta.__tostring(self)
  if self.__variant == 'primitive' then
    return self.name
  else
    error('Stop right there, criminal scum!')
  end
end

function cc.hard_type_primitive(name)
  assert(type(name) == 'string')
  return setmetatable({
    __variant = 'primitive',
    name = name,
  }, hard_type_meta)
end

function cc.hard_type_array(element_type, quantity)
  assert(getmetatable(element_type) == hard_type_meta)
  assert(type(quantity) == 'number')
  return setmetatable({
    __variant = 'array',
    element_type = element_type,
    quantity = quantity,
  }, hard_type_meta)
end

function cc.is_hard_type(hard_type)
  return getmetatable(hard_type) == hard_type_meta
end

----------------------------------------------------------------------------------------------------
-- cc.variable_type
--
-- Defines a reference/const qualified type of a variable

function variable_type_meta.__tostring(self)
  local str = ''
  if self.const then
    str = 'const '
  end
  str = str..tostring(self.hard_type)
  if self.reference then
    str = str..' &'
  end
  return str
end

function cc.variable_type(const, hard_type, reference)
  assert(type(const) == 'boolean')
  assert(getmetatable(hard_type) == hard_type_meta)
  assert(type(reference) == 'boolean')
  return setmetatable({
    const = const,
    hard_type = hard_type,
    reference = reference,
  }, variable_type_meta)
end

function cc.is_variable_type(var_type)
  return getmetatable(var_type) == variable_type_meta
end

----------------------------------------------------------------------------------------------------
-- cc.variable

function variable_meta.__tostring(self)
  return tostring(self.type)..' in '..tostring(self.ir_id)
end

function cc.variable(type_, ir_id)
  assert(type(type_) == 'table')
  assert(type(ir_id) == 'string')
  return setmetatable({
    type = type_,
    ir_id = ir_id
  }, variable_meta)
end

function cc.is_variable(var)
  return getmetatable(var) == variable_meta
end

----------------------------------------------------------------------------------------------------
-- cc.function_

function cc.function_(name, argument_types, return_types, ir_subr)
  assert(type(name) == 'string')
  for k,v in pairs(argument_types) do assert(cc.is_variable_type(v)) end
  for k,v in pairs(return_types) do assert(cc.is_variable_type(v)) end
  assert(ir.is_subroutine(ir_subr))
  return setmetatable({
    name = name,
    argument_types = argument_types,
    return_types = return_types,
    ir_subroutine = ir_subr,
  }, function_meta)
end

function cc.is_function(func)
  return getmetatable(func) == function_meta
end

----------------------------------------------------------------------------------------------------
-- cc.expression_proxy
--
-- An expression proxy might be a literal, or an lvalue.
-- Keep in mind, it may be behind you.

-- Creates an expression proxy for a literal
-- For now, literals are only integers
function cc.expression_proxy_literal(value)
  assert(type(value) == 'number')
  return setmetatable({
    __variant = 'literal',
    value = value,
  }, expression_proxy_meta)
end

-- Creates an expression proxy for an lvalue
function cc.expression_proxy_lvalue(lvalue)
  assert(getmetatable(lvalue) == lvalue_proxy_meta)
  return setmetatable({
    __variant = 'lvalue',
    lvalue = lvalue,
  }, expression_proxy_meta)
end

function cc.is_expression_proxy(expr)
  return getmetatable(expr) == expression_proxy_meta
end

----------------------------------------------------------------------------------------------------
-- cc.lvalue_proxy
--
-- An lvalue proxy might be a variable, or an element of an array.

-- Creates a variable lvalue proxy
function cc.lvalue_proxy_variable(var, temp)
  assert(getmetatable(var) == variable_meta)
  assert(type(temp) == 'boolean')
  return setmetatable({
    __variant = 'variable',
    variable = var,
    temporary = temp
  }, lvalue_proxy_meta)
end

-- Creates an array element lvalue proxy
function cc.lvalue_proxy_array_elem(var, expr)
  assert(getmetatable(var) == variable_meta)
  assert(getmetatable(expr) == expression_proxy_meta)
  return setmetatable({
    __variant = 'array_element',
    variable = var,
    expression = expr
  }, lvalue_proxy_meta)
end

function cc.is_lvalue_proxy(lvalue)
  return getmetatable(lvalue) == lvalue_proxy_meta
end

----------------------------------------------------------------------------------------------------
-- cc.block_stack
--
-- Implements a data structure which conveniently tracks stack allocations and local variable scope
-- during AST traversal

function block_stack_meta:__len()
  return self.depth
end

function block_stack_methods:enter_block()
  local top_block = self.top_block
  -- Create a new block table, inheriting the top (current) block's stack index and scope
  new_block = { parent = top_block,
                stack_index = top_block.stack_index,
                locals = { } }
  -- This is the top now
  self.top_block = new_block
  self.depth = self.depth + 1
  -- Return to verify correct exit
  return new_block
end

function block_stack_methods:exit_block(given_block)
  local top_block = self.top_block
  local parent = top_block.parent
  -- Ensure that there is a block to free
  assert(parent, 'Unbalanced enter/exit')
  -- Optionally ensure that we're exiting the expected block
  if given_block then assert(top_block == given_block, 'Unbalanced enter/exit') end
  -- Remove top block
  self.top_block = parent
  self.depth = self.depth - 1
end

function block_stack_methods:stack_alloc(size)
  assert(type(size) == 'number')
  local block = self.top_block
  -- Increment stack_index by size
  local offset = block.stack_index
  block.stack_index = block.stack_index + size
  -- Return new allocation
  return offset, size
end

function block_stack_methods:add_variable(var)
  assert(getmetatable(var) == variable_meta)
  -- Append to list of locals
  local locals = self.top_block.locals
  locals[#locals + 1] = var
end

function block_stack_methods:name_variable(name, var)
  assert(type(name) == 'string')
  assert(getmetatable(var) == variable_meta)
  -- Insert in map of locals
  local locals = self.top_block.locals
  locals[name] = var
end

function block_stack_methods:find_variable(name)
  assert(type(name) == 'string')
  -- Start at the top, and work our way down
  local block = self.top_block
  while block do
    -- Return if found in this block
    local var = block.locals[name]
    if var then
      return var
    end
    -- Continue to parent of this block
    block = block.parent
  end
end

function cc.block_stack()
  return setmetatable({
    top_block = { locals = { }, stack_index = 0 },
    depth = 0,
  }, block_stack_meta)
end

----------------------------------------------------------------------------------------------------
-- cc.module_stack
--

function module_stack_methods:enter_module(name)
  local top_module = self.top_module
  -- Try to re-enter submodules
  local next_module = top_module.submodules[name]
  -- Create a new child module if needed
  if not next_module then
    next_module = { parent = top_module,
                    name = name,
                    submodules = { },
                    functions = { } }
    top_module.submodules[name] = next_module
  end
  -- This is the top now
  self.top_module = next_module
  -- Add name to current path
  self.name_path[#self.name_path+1] = name
  -- Return to verify correct exit
  return next_module
end

function module_stack_methods:exit_module(given_module)
  local top_module = self.top_module
  local parent = top_module.parent
  -- Ensure that there is a module to free
  assert(parent, 'Unbalanced enter/exit')
  -- Optionally ensure that we're exiting the expected module
  if given_module then assert(top_module == given_module, 'Unbalanced enter/exit') end
  -- Remove top module
  self.top_module = parent
  -- Remove last name from current path
  self.name_path[#self.name_path] = nil
end

local function function_search(module, name_path)
  local mod = module
  local nmods = #name_path-1
  for i=1,nmods do
    mod = mod.submodules[name_path[i]]
    if not mod then
      -- This path doesn't exist starting from this module.
      -- Search parent
      return function_search(module.parent, name_path)
    end
  end
  local func = mod.functions[name_path[#name_path]]
  if func then
    -- Found!
    return func
  else
    -- This path exists, but the function doesn't.
    -- Search parent
    return function_search(module.parent, name_path)
  end
end

function module_stack_methods:find_function(name_path)
  return function_search(self.top_module, name_path)
end

function module_stack_methods:find_function_local(name)
  return self.top_module.functions[name]
end

function module_stack_methods:add_function(name, func)
  assert(type(name) == 'string')
  assert(getmetatable(func) == function_meta)
  self.top_module.functions[name] = func
end

function cc.module_stack()
  local global_module = { submodules = { }, functions = { } }
  return setmetatable({
    name_path = { },
    global_module = global_module,
    top_module = global_module,
  }, module_stack_meta)
end

function cc.dump_modules(module_stack)
  local function dump_module(module, level)
    local indent = string.rep('  ', level)
    if module.name then
      io.write(indent..module.name..' {\n')
    else
      io.write(indent..'{\n')
    end
    for k,sub in pairs(module.submodules) do
      dump_module(sub, level+1)
    end
    for k,func in pairs(module.functions) do
      io.write(indent..'  function '..func.name..'()\n')
    end
    io.write(indent..'}\n')
  end

  for k,sub in pairs(module_stack.global_module.submodules) do
    dump_module(sub, 0)
  end
end

----------------------------------------------------------------------------------------------------
-- Built-in types
--

cc.type_int64 = cc.hard_type_primitive('int64')

return cc

