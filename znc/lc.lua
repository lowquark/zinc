
----------------------------------------------------------------------------------------------------
-- Language constructions

local lc = { }

----------------------------------------------------------------------------------------------------
-- lc.hard_type
--
-- Defines pure data, no references, no const

lc.hard_type_methods = { }
lc.hard_type_meta = { __index = lc.hard_type_methods }
function lc.hard_type_meta.__tostring(self)
  if self.__variant == 'primitive' then
    return '( primitive hard_type '..self.name..' )'
  elseif self.__variant == 'struct' then
    return '( struct hard_type )'
  else
    return '( '..self.__variant..' hard_type ? )'
  end
end
function lc.hard_type_primitive(name)
  assert(type(name) == 'string')
  return setmetatable({
    __variant = 'primitive',
    name = name,
  }, lc.hard_type_meta)
end
function lc.hard_type_struct(fields)
  assert(type(fields) == 'table')
  for i,f in ipairs(fields) do
    assert(getmetatable(f.type) == lc.hard_type_meta)
    assert(getmetatable(f.name) == 'string')
  end
  return setmetatable({
    __variant = 'struct',
    fields = deepcopy(fields)
  }, lc.hard_type_meta)
end
function lc.hard_type_array(var_type, quantity)
  assert(getmetatable(var_type) == lc.hard_type_meta)
  assert(type(quantity) == 'number')
  return setmetatable({
    __variant = 'array',
    element_type = element_type,
    quantity = quantity,
  }, lc.hard_type_meta)
end

lc.type_int64 = lc.hard_type_primitive('int64')

----------------------------------------------------------------------------------------------------
-- lc.variable_type
--
-- Defines a reference/const qualified type of a variable

lc.variable_type_methods = { }
lc.variable_type_meta = { __index = lc.variable_type_methods }
function lc.variable_type_meta.__tostring(self)
  local str = '( var_type '
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
function lc.variable_type(const, hard_type, reference)
  assert(type(const) == 'boolean')
  assert(getmetatable(hard_type) == lc.hard_type_meta)
  assert(type(reference) == 'boolean')
  return setmetatable({
    const = const,
    hard_type = hard_type,
    reference = reference,
  }, lc.variable_type_meta)
end

----------------------------------------------------------------------------------------------------
-- lc.variable

function lc.variable(var_type, ir_id)
  assert(type(var_type) == 'table')
  assert(type(ir_id) == 'string')
  return { type = 'local',
           variable_type = var_type,
           ir_id = ir_id }
end

----------------------------------------------------------------------------------------------------
-- lc.function_

function lc.function_(name, ast_func, ir_subr)
  assert(type(name) == 'string')
  assert(type(ast_func) == 'table')
  assert(type(ir_subr) == 'table')
  return { name = name,
           ast_function = ast_func,
           ir_subroutine = ir_subr }
end

return lc

