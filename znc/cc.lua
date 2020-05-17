
----------------------------------------------------------------------------------------------------
-- cc - (c)ompiler (c)onstructions

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

----------------------------------------------------------------------------------------------------
-- cc.variable

cc.variable_methods = { }
cc.variable_meta = { __index = cc.variable_methods }
function cc.variable(var_type, ir_id)
  assert(type(var_type) == 'table')
  assert(type(ir_id) == 'string')
  return setmetatable({
    type = 'local',
    variable_type = var_type,
    ir_id = ir_id
  }, cc.variable_meta)
end

----------------------------------------------------------------------------------------------------
-- cc.function_

cc.function_methods = { }
cc.function_meta = { __index = cc.function_methods }
function cc.function_(name, ast_func, ir_subr)
  assert(type(name) == 'string')
  assert(type(ast_func) == 'table')
  assert(type(ir_subr) == 'table')
  return setmetatable({
    name = name,
    ast_function = ast_func,
    ir_subroutine = ir_subr
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

return cc

