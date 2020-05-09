
----------------------------------------------------------------------------------------------------
-- AST - (A)bstract (S)yntax (T)ree utilities
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Tree construction

ast = { }

function ast.name_path()
  return { }
end

function ast.type_specifier(name_path, const, ref, quantity)
  assert(name_path)
  assert(type(const) == 'boolean')
  assert(type(ref) == 'boolean')
  assert(not quantity or type(quantity) == 'number')
  return { name_path = name_path, const = const, ref = ref, quantity = quantity }
end

function ast.lvalue_declaration(type_spec, name)
  assert(type_spec)
  assert(name)
  return { type = 'declaration', type_specifier = type_spec, name = name }
end

function ast.lvalue_reference(name_path, index_exprs)
  assert(type(name_path) == 'table')
  assert(not index_exprs or type(index_exprs) == 'table')
  return { type = 'reference', name_path = name_path, index_expressions = index_exprs }
end

-- Expressions: (The list is long)
function ast.expr_integer(value)
  assert(value)
  return { type = 'integer', value = value }
end
function ast.expr_variable(name, index_exprs)
  assert(type(name) == 'string')
  assert(not index_exprs or type(index_exprs) == 'table')
  return { type = 'variable', name = name, index_expressions = index_exprs }
end

function ast.expr_negate(subexpr)
  assert(subexpr)
  return { type = 'negate', expression = subexpr }
end
function ast.expr_binnot(subexpr)
  assert(subexpr)
  return { type = 'binnot', expression = subexpr }
end
function ast.expr_lognot(subexpr)
  assert(subexpr)
  return { type = 'lognot', expression = subexpr }
end

function ast.expr_add(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'add',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_sub(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'sub',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_mul(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'mul',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_div(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'div',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end

function ast.expr_cmpeq(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'cmpeq',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_cmpneq(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'cmpneq',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_cmplt(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'cmplt',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_cmpgt(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'cmpgt',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_cmpleq(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'cmpleq',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_cmpgeq(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'cmpgeq',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end

function ast.expr_logand(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'logand',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end
function ast.expr_logor(subexpr_a, subexpr_b)
  assert(subexpr_a)
  assert(subexpr_b)
  return { type = 'logor',
           expression_a = subexpr_a,
           expression_b = subexpr_b }
end

function ast.expr_call(name_path, args)
  assert(name_path)
  assert(args)
  return { type = 'call',
           name_path = name_path,
           arguments = args }
end

-- Statements: block | local | if | return | assign | call
function ast.stmt_block()
  return { type = 'block' }
end
function ast.stmt_local_declaration(type_name_path, name)
  assert(type_name_path)
  assert(name)
  return { type = 'local',
           type_name_path = type_name_path,
           name = name }
end
function ast.stmt_if(expr, if_stmt, else_stmt)
  assert(expr)
  assert(if_stmt)
  return { type = 'if', expression = expr, if_statement = if_stmt, else_statement = else_stmt }
end
function ast.stmt_return(exprs)
  assert(exprs)
  return { type = 'return', expressions = exprs }
end
function ast.stmt_assign(name, expr)
  assert(name)
  assert(expr)
  return { type = 'assign',
           name = name,
           expression = expr }
end
function ast.stmt_assignment(lvalue_list, expr_list)
  assert(lvalue_list)
  assert(expr_list)
  return { type = 'assignment',
           lvalues = lvalue_list,
           expressions = expr_list }
end
function ast.stmt_function_call(name_path, args)
  assert(name_path)
  assert(args)
  return { type = 'call',
           name_path = name_path,
           arguments = args }
end

-- Struct declarations: field | access
function ast.struct_field_declaration(type_name_path, name)
  assert(type_name_path)
  assert(name)
  return { type = 'field',
           type_name_path = type_name_path,
           name = name }
end
function ast.struct_access_declaration(module_name_path)
  assert(module_name_path)
  return { type = 'access', name_path = module_name_path }
end

-- Module declarations: member | function
function ast.member_declaration(type_name_path, name)
  assert(type_name_path)
  assert(name)
  return { type = 'member',
           type_name_path = type_name_path,
           name = name }
end
function ast.function_declaration(name, arguments, returns, block)
  assert(name)
  assert(arguments)
  assert(returns)
  assert(block)
  return { type = 'function',
           name = name,
           arguments = arguments,
           returns = returns,
           block = block }
end

-- File-scope declarations: struct | module
function ast.struct_declaration(name_path, declarations)
  assert(name_path)
  assert(declarations)
  return { type = 'struct',
           name_path = name_path,
           declarations = declarations }
end
function ast.module_declaration(name_path, declarations)
  assert(name_path)
  assert(declarations)
  return { type = 'module',
           name_path = name_path,
           declarations = declarations }
end

----------------------------------------------------------------------------------------------------
-- Pretty printing

local dump_expr
local dump_stmt

local function type_spec_str(type_spec)
  local str = ''
  if type_spec.const then
    str = str..'const '
  end
  str = str..table.concat(type_spec.name_path, ':')
  if type_spec.quantity then
    str = str..' ['..tostring(type_spec.quantity)..']'
  end
  if type_spec.ref then
    str = str..' &'
  end
  return str
end

function dump_lvalue(lvalue, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  if lvalue.type == 'declaration' then
    io.write(indent..'DECLARATION '..type_spec_str(lvalue.type_specifier)..' '..lvalue.name..'\n')
  elseif lvalue.type == 'reference' then
    io.write(indent..'REFERENCE '..table.concat(lvalue.name_path, ':')..'\n')
  else
    error('unknown lvalue type `'..lvalue.type..'`')
  end
end

function dump_expr(expr, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  local indent2 = string.rep('  ', level+1)
  io.write(indent..string.upper(expr.type)..' ')
  if expr.type == 'call' then
    io.write(table.concat(expr.name_path, ':')..'\n')
    for k,arg_expr in ipairs(expr.arguments) do
      dump_expr(arg_expr, level+1)
    end
  elseif expr.type == 'variable' then
    io.write(expr.name..'\n')
    if expr.index_expressions then
      for i,idx_expr in ipairs(expr.index_expressions) do
        io.write(indent2..'INDEX\n')
        dump_expr(idx_expr, level+2)
      end
    end
  else
    if expr.value then
      io.write(tostring(expr.value)..'\n')
    elseif expr.name then
      io.write(expr.name..'\n')
    else
      io.write('\n')
    end
    if expr.expression then
      dump_expr(expr.expression, level+1)
    elseif expr.expression_a and expr.expression_b then
      dump_expr(expr.expression_a, level+1)
      dump_expr(expr.expression_b, level+1)
    end
  end
end

function dump_stmt(stmt, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  local indent2 = string.rep('  ', level+1)
  if stmt.type == 'return' then
    io.write(indent..'RETURN\n')
    for i,ret_expr in ipairs(stmt.expressions) do
      dump_expr(ret_expr, level+1)
    end
  elseif stmt.type == 'assignment' then
    io.write(indent..'ASSIGNMENT\n')
    for i,lvalue in ipairs(stmt.lvalues) do
      dump_lvalue(lvalue, level+1)
    end
    if #stmt.expressions > 0 then
      io.write(indent2..':=\n')
      for i,expr in ipairs(stmt.expressions) do
        dump_expr(expr, level+2)
      end
    end
  elseif stmt.type == 'local' then
    io.write(indent..'LOCAL '..stmt.name..' : '..table.concat(stmt.type_name_path,':')..'\n')
  elseif stmt.type == 'if' then
    io.write(indent..'IF\n')
    dump_expr(stmt.expression, level+1)
    io.write(indent..'THEN\n')
    dump_stmt(stmt.if_statement, level+1)
    if stmt.else_statement then
      io.write(indent..'ELSE\n')
      dump_stmt(stmt.else_statement, level+1)
    end
  elseif stmt.type == 'block' then
    io.write(indent..'BLOCK\n')
    for i,block_stmt in ipairs(stmt) do
      dump_stmt(block_stmt, level+1)
    end
  elseif stmt.type == 'call' then
    io.write(indent..'CALL '..table.concat(stmt.name_path,':')..'\n')
    for i,arg_expr in ipairs(stmt.arguments) do
      dump_expr(arg_expr, level+1)
    end
  end
end

local function dump_module_decl(decl, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  if decl.type == 'function' then
    io.write(indent..'FUNCTION '..decl.name..' (')
    for i,arg in ipairs(decl.arguments) do
      io.write(type_spec_str(arg.type_specifier)..' '..arg.name)
      if i ~= #decl.arguments then
        io.write(', ')
      end
    end
    io.write(') -> (')
    for i,ret in ipairs(decl.returns) do
      io.write(type_spec_str(ret.type_specifier)..' '..ret.name)
      if i ~= #decl.returns then
        io.write(', ')
      end
    end
    io.write(')\n')
    dump_stmt(decl.block, level + 1)
  elseif decl.type == 'member' then
    io.write(indent..'MEMBER '..decl.name..' : '..table.concat(decl.type_name_path,':')..'\n')
  else
    error('unknown decl declaration type `'..decl.type..'`')
  end
end

local function dump_module(module, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  io.write(indent..'MODULE '..table.concat(module.name_path,':')..'\n')
  for i,decl in ipairs(module.declarations) do
    dump_module_decl(decl, level + 1)
  end
end

local function dump_struct_decl(decl, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  if decl.type == 'access' then
    io.write(indent..'ACCESS '..table.concat(decl.name_path,':')..'\n')
  elseif decl.type == 'field' then
    io.write(indent..'FIELD '..decl.name..' : '..table.concat(decl.type_name_path,':')..'\n')
  else
    error('unknown struct declaration type `'..decl.type..'`')
  end
end

local function dump_struct(struct, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  io.write(indent..'STRUCT '..table.concat(struct.name_path,':')..'\n')
  for i,decl in ipairs(struct.declarations) do
    dump_struct_decl(decl, level + 1)
  end
end

local function dump_file_decl(decl, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  if decl.type == 'struct' then
    dump_struct(decl, level)
  elseif decl.type == 'module' then
    dump_module(decl, level)
  else
    error('unknown file-scope declaration type '..decl.type)
  end
end

function ast.dump(ast, level)
  for i,decl in ipairs(ast) do
    dump_file_decl(decl, 0)
  end
end

return ast

