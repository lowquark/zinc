
----------------------------------------------------------------------------------------------------
-- cc - (c)ompiler (c)onstructions
----------------------------------------------------------------------------------------------------

local ir = require 'ir'
local pprint = require 'pprint'

local function assert_type(val, typestr, ...)
  assert(type(val) == typestr, ...)
end

local function assert_meta_type(t, metatable, ...)
  assert(getmetatable(t) == metatable, ...)
end

local cc = { }

----------------------------------------------------------------------------------------------------
-- cc.hard_type
--
-- Defines pure data, no references, no const

cc.hard_type_methods = { }
cc.hard_type_meta = { __index = cc.hard_type_methods }

function cc.hard_type_meta.__tostring(self)
  if self.__variant == 'primitive' then
    return '( primitive hard_type '..self.name..' )'
  elseif self.__variant == 'struct' then
    return '( struct hard_type )'
  else
    return '( '..self.__variant..' hard_type ? )'
  end
end

function cc.hard_type_primitive(name)
  assert(type(name) == 'string')
  return setmetatable({
    __variant = 'primitive',
    name = name,
  }, cc.hard_type_meta)
end

function cc.hard_type_struct(fields)
  assert(type(fields) == 'table')
  for i,f in ipairs(fields) do
    assert(getmetatable(f.type) == cc.hard_type_meta)
    assert(getmetatable(f.name) == 'string')
  end
  return setmetatable({
    __variant = 'struct',
    fields = deepcopy(fields)
  }, cc.hard_type_meta)
end

function cc.hard_type_array(element_type, quantity)
  assert(getmetatable(element_type) == cc.hard_type_meta)
  assert(type(quantity) == 'number')
  return setmetatable({
    __variant = 'array',
    element_type = element_type,
    quantity = quantity,
  }, cc.hard_type_meta)
end

cc.type_int64 = cc.hard_type_primitive('int64')

----------------------------------------------------------------------------------------------------
-- cc.variable_type
--
-- Defines a reference/const qualified type of a variable

cc.variable_type_methods = { }
cc.variable_type_meta = { __index = cc.variable_type_methods }

function cc.variable_type_meta.__tostring(self)
  local str = '( variable_type '
  if self.const then
    str = str..'const '
  end
  str = str..tostring(self.hard_type)..' '
  if self.reference then
    str = str..'& '
  end
  str = str..')'
  return str
end

function cc.variable_type(const, hard_type, reference)
  assert(type(const) == 'boolean')
  assert(getmetatable(hard_type) == cc.hard_type_meta)
  assert(type(reference) == 'boolean')
  return setmetatable({
    const = const,
    hard_type = hard_type,
    reference = reference,
  }, cc.variable_type_meta)
end

function cc.is_variable_type(var_type)
  return getmetatable(var_type) == cc.variable_type_meta
end

----------------------------------------------------------------------------------------------------
-- cc.variable

cc.variable_methods = { }
cc.variable_meta = { __index = cc.variable_methods }

function cc.variable_meta.__tostring(self)
  return '( variable '..tostring(self.type)..' -> '..tostring(self.ir_id)..' )'
end

function cc.variable(type_, ir_id)
  assert(type(type_) == 'table')
  assert(type(ir_id) == 'string')
  return setmetatable({
    type = 'local',
    type = type_,
    ir_id = ir_id
  }, cc.variable_meta)
end

function cc.is_variable(var)
  return getmetatable(var) == cc.variable_meta
end

----------------------------------------------------------------------------------------------------
-- cc.function_

cc.function_methods = { }
cc.function_meta = { __index = cc.function_methods }

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
  }, cc.function_meta)
end

----------------------------------------------------------------------------------------------------
-- cc.expression_proxy
--
-- An expression proxy might be a literal, or an lvalue.
-- Keep in mind, it may be behind you.

cc.expression_proxy_methods = { }
cc.expression_proxy_meta = { __index = cc.expression_proxy_methods }

-- Creates an expression proxy for a literal
-- For now, literals are only integers
function cc.expression_proxy_literal(value)
  assert_type(value, 'number')
  return setmetatable({
    __variant = 'literal',
    value = value,
  }, cc.expression_proxy_meta)
end

-- Creates an expression proxy for an lvalue
function cc.expression_proxy_lvalue(lvalue)
  assert_meta_type(lvalue, cc.lvalue_proxy_meta)
  return setmetatable({
    __variant = 'lvalue',
    lvalue = lvalue,
  }, cc.expression_proxy_meta)
end

----------------------------------------------------------------------------------------------------
-- cc.lvalue_proxy
--
-- An lvalue proxy might be a variable, or an element of an array.

cc.lvalue_proxy_methods = { }
cc.lvalue_proxy_meta = { __index = cc.lvalue_proxy_methods }

-- Creates a variable lvalue proxy
function cc.lvalue_proxy_variable(var, temp)
  assert_meta_type(var, cc.variable_meta)
  assert_type(temp, 'boolean')
  return setmetatable({
    __variant = 'variable',
    variable = var,
    temporary = temp
  }, cc.lvalue_proxy_meta)
end

-- Creates an array element lvalue proxy
function cc.lvalue_proxy_array_elem(var, expr)
  assert_meta_type(var, cc.variable_meta)
  assert_meta_type(expr, cc.expression_proxy_meta)
  return setmetatable({
    __variant = 'array_element',
    variable = var,
    expression = expr
  }, cc.lvalue_proxy_meta)
end

----------------------------------------------------------------------------------------------------
-- cc.block_stack
--
-- Implements a data structure which conveniently tracks stack allocations and local variable scope
-- during AST traversal

cc.block_stack_methods = { }
cc.block_stack_meta = { __index = cc.block_stack_methods }

function cc.block_stack_methods:enter_block()
  local bottom_block = self.bottom_block
  -- Create a new block, inheriting the bottom (current) block's stack index and scope
  new_block = { parent = bottom_block,
                stack_index = bottom_block.stack_index,
                locals = { } }
  -- This is the bottom now
  self.bottom_block = new_block
  -- Return to verify correct exit
  return new_block
end

function cc.block_stack_methods:exit_block(given_block)
  assert(type(given_block) == 'table')
  local bottom_block = self.bottom_block
  local parent = bottom_block.parent
  -- Ensure that there is a block to free
  assert(parent, 'Unbalanced enter/exit')
  -- Optionally ensure that we're exiting the expected block
  if given_block then assert(bottom_block == given_block, 'Unbalanced enter/exit') end
  -- Remove top block
  self.bottom_block = parent
end

function cc.block_stack_methods:stack_alloc(size)
  assert(type(size) == 'number')
  local block = self.bottom_block
  -- Increment stack_index by size
  local offset = block.stack_index
  block.stack_index = block.stack_index + size
  -- Return new allocation
  return offset, size
end

function cc.block_stack_methods:add_variable(var)
  assert(getmetatable(var) == cc.variable_meta)
  -- Append to list of locals
  local locals = self.bottom_block.locals
  locals[#locals + 1] = var
end

function cc.block_stack_methods:name_variable(name, var)
  assert(type(name) == 'string')
  assert(getmetatable(var) == cc.variable_meta)
  -- Insert in map of locals
  local locals = self.bottom_block.locals
  locals[name] = var
end

function cc.block_stack_methods:find_variable(name)
  assert(type(name) == 'string')
  -- Start at the bottom, and work our way out
  local block = self.bottom_block
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
    bottom_block = { locals = { }, stack_index = 0 },
  }, cc.block_stack_meta)
end

return cc

