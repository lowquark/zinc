
----------------------------------------------------------------------------------------------------
-- AST - (A)bstract (S)yntax (T)ree utilities
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Tree construction

ast = { }

function ast.name_path()
  return { }
end

-- Expressions: (The list is long)
function ast.expr_integer(value)
  assert(value)
  return { type = 'integer', value = value }
end
function ast.expr_variable(name)
  assert(name)
  return { type = 'variable', name = name }
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
function ast.stmt_return(expr)
  assert(expr)
  return { type = 'return', expression = expr }
end
function ast.stmt_assign(name, expr)
  assert(name)
  assert(expr)
  return { type = 'assign',
           name = name,
           expression = expr }
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

dump_expr = function(expr, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  io.write(indent..string.upper(expr.type)..' ')
  if expr.type == 'call' then
    io.write(table.concat(expr.name_path, ':')..'\n')
    for k,arg_expr in ipairs(expr.arguments) do
      dump_expr(arg_expr, level+1)
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

dump_stmt = function(stmt, level)
  local level = level or 0
  local indent = string.rep('  ', level)
  if stmt.type == 'return' then
    io.write(indent..'RETURN\n')
    if stmt.expression then
      dump_expr(stmt.expression, level+1)
    end
  elseif stmt.type == 'assign' then
    io.write(indent..'ASSIGN '..stmt.name..'\n')
    dump_expr(stmt.expression, level+1)
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
    io.write(indent..'FUNCTION '..decl.name..' ( ')
    for i,arg in ipairs(decl.arguments) do
      io.write(arg.name..':'..table.concat(arg.type_name_path,':')..' ')
    end
    io.write(') -> ( ')
    for i,arg in ipairs(decl.returns) do
      io.write(arg.name..':'..table.concat(arg.type_name_path,':')..' ')
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
