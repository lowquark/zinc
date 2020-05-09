
----------------------------------------------------------------------------------------------------
-- Parse a file and generate an AST
----------------------------------------------------------------------------------------------------

local ast = require 'ast'
local Lexer = require 'Lexer'

local token_type_strings = {
  [ 'eof' ]       = 'EOF',
  [ 'name' ]      = 'name',
  [ 'access' ]    = 'access keyword',
  [ 'struct' ]    = 'struct keyword',
  [ 'module' ]    = 'module keyword',
  [ 'function' ]  = 'function keyword',
  [ 'return' ]    = 'return keyword',
  [ 'if' ]        = 'if keyword',
  [ 'else' ]      = 'else keyword',
  [ 'integer' ]   = 'integer constant',
  [ 'lcurly' ]    = '`{`',
  [ 'rcurly' ]    = '`}`',
  [ 'lparen' ]    = '`(`',
  [ 'rparen' ]    = '`)`',
  [ 'lsquare' ]   = '`[`',
  [ 'rsquare' ]   = '`]`',
  [ 'integer' ]   = 'integer constant',
  [ 'semicolon' ] = '`;`',
  [ 'dot' ]       = '`.`',
  [ 'plus' ]      = '`+`',
  [ 'minus' ]     = '`-`',
  [ 'asterisk' ]  = '`*`',
  [ 'fslash' ]    = '`/`',
  [ 'tilde' ]     = '`~`',
  [ 'cmpneq' ]    = '`!=`',
  [ 'lognot' ]    = '`!`',
  [ 'cmpleq' ]    = '`<=`',
  [ 'cmplt' ]     = '`<`',
  [ 'cmpgeq' ]    = '`>=`',
  [ 'cmpgt' ]     = '`>`',
  [ 'logand' ]    = '`&&`',
  [ 'binand' ]    = '`&`',
  [ 'logor' ]     = '`||`',
  [ 'binor' ]     = '`|`',
  [ 'cmpeq' ]     = '`==`',
  [ 'equals' ]    = '`=`',
}

-- Prints a parse error message and exits.
-- The printed error message contains the line and column number of the current lexer position, and
-- indicates which token was unexpected and which (given) tokens would have been valid.
--     L : Lexer state
--   ... : Valid token type strings
local function parse_abort_expected(L, ...)
  local expstr = ''
  local n = select('#', ...)
  for i=1,n do
    local v = select(i, ...)
    expstr = expstr..(token_type_strings[v] or v)
    if i == n - 1 then
      expstr = expstr..' or '
    elseif i ~= n then
      expstr = expstr..', '
    end
  end
  local actstr = token_type_strings[L.next.type]
  if not actstr then
    actstr = 'unknown token'
  end
  io.write('Error on line '..L.line..', col '..L.col..': Expected '..expstr..'; got '..actstr..'\n')
  io.write(debug.traceback())
  os.exit(3)
end

-- Forward declarations of certain functions
local parse_expression
local expect_expression
local expect_statement
local parse_if_statement

-- Unless otherwise specified,
--   * Functions of the form expect_xxxx throw an error if the next token is not valid to start rule
--   xxxx.
--   * Functions of the form parse_xxxx return nil if the next token is not valid to start rule
--   xxxx.

-- Tries to parse a token of the given type
local function parse_token(L, type)
  if L.next.type == type then
    L:read()
    return true
  else
    return false
  end
end

-- Expects to parse a token of the given type
local function expect_token(L, type)
  if not parse_token(L, type) then
    parse_abort_expected(L, type)
  end
  return true
end

-- Tries to parse a <name> token
local function parse_name(L)
  if L.next.type == 'name' then
    local name = L.next.value
    L:read()
    return name
  end
end

-- Expects to parse a <name> token
local function expect_name(L)
  local name = parse_name(L)
  if not name then
    parse_abort_expected(L, 'name')
  end
  return name
end

-- Tries to parse an <integer> token
local function parse_integer(L)
  if L.next.type == 'integer' then
    local val = L.next.value
    L:read()
    return tonumber(val)
  end
end

-- Expects to parse an <integer> token
local function expect_integer(L)
  local val = parse_integer(L)
  if not val then
    parse_abort_expected(L, 'integer')
  end
  return val
end

-- Tries to parse the rule: <name-path-rest>
-- Returns the AST object for a name path, using leading_name as the top-level name
local function parse_name_path_rest(L, leading_name)
  local name_path = ast.name_path()
  table.insert(name_path, leading_name)
  while parse_token(L, 'colon') do
    table.insert(name_path, expect_name(L))
  end
  return name_path
end

-- Tries to parse the rule: <name-path>
-- Returns the AST object for a name path
local function parse_name_path(L)
  local name = parse_name(L)
  if name then
    return parse_name_path_rest(L, name)
  end
end

-- Expects to parse the rule: <name-path>
-- Returns the AST object for a name path
local function expect_name_path(L)
  return parse_name_path_rest(L, expect_name(L))
end

-- Tries to parse the rule: <lvalue>
-- Returns the AST object for an lvalue
local function parse_lvalue(L)
  -- <name-path> [ '[' <integer> ']' ]
  local name_path = parse_name_path(L)
  if name_path then
    local index_exprs
    while parse_token(L, 'lsquare') do
      -- Index expression
      index_exprs = index_exprs or { }
      index_exprs[#index_exprs + 1] = expect_expression(L)
      expect_token(L, 'rsquare')
    end
    -- Name paths are currently accepted by the parser but they have no meaning
    if #name_path ~= 1 then error('Absolute variable names are unsupported as of yet.') end
    return ast.lvalue(name_path, index_exprs)
  end
end

-- Tries to parse the rule: <struct-access-declaration>
-- Returns the AST object for a struct access declaration
local function parse_struct_access_declaration(L)
  if parse_token(L, 'access') then
    local name_path = expect_name_path(L)
    expect_token(L, 'semicolon')
    return ast.struct_access_declaration(name_path)
  end
end

-- Tries to parse the rule: <struct-field-declaration>
-- Returns the AST object for a struct field declaration
local function parse_struct_field_declaration(L)
  local type_name_path = parse_name_path(L)
  if type_name_path then
    local name = expect_name(L)
    expect_token(L, 'semicolon')
    return ast.struct_field_declaration(type_name_path, name)
  end
end

-- Expects to parse the rule: <struct-access-declaration>
-- Returns the appropriate AST object for the declaration
local function expect_struct_body_declaration(L)
  local decl = parse_struct_access_declaration(L)
  if decl then return decl end
  decl = parse_struct_field_declaration(L)
  if decl then return decl end
  parse_abort_expected(L, 'field/access declaration')
end

-- Tries to parse the rule: <struct-declaration>
-- Returns the AST object for a struct declaration
local function parse_struct_declaration(L)
  local name_path 
  local declarations = {}
  if parse_token(L, 'struct') then
    local name_path = expect_name_path(L)
    expect_token(L, 'lcurly')
    while not parse_token(L, 'rcurly') do
      table.insert(declarations, expect_struct_body_declaration(L))
    end
    return ast.struct_declaration(name_path, declarations)
  end
end

-- Tries to parse the rule: <type-specifier>
-- Returns the AST object for a type specifier
local function parse_type_specifier(L)
  local st = L:state()
  local const = parse_token(L, 'const')
  local name_path = parse_name_path(L)
  local ref
  local quantity
  if not name_path then
    -- Not actually a type, boo
    L:reset(st)
    return
  end
  if parse_token(L, 'lsquare') then
    quantity = parse_integer(L)
    expect_token(L, 'rsquare')
  end
  ref = parse_token(L, 'binand')
  return ast.type_specifier(name_path, const, ref, quantity)
end

-- Expects to parse the rule: <type>
-- Returns the AST object for a type
local function expect_type_specifier(L)
  local t = parse_type_specifier(L)
  if t then
    return t
  else
    parse_abort_expected(L, 'type specifier')
  end
end

-- Tries to parse: [ <argument-list> ]
-- Returns the AST object for an argument list
-- Returns an empty list if no expressions could be read
local function parse_argument_list(L)
  local list = {}
  local expr = parse_expression(L)
  if expr then
    table.insert(list, expr)
    while parse_token(L, 'comma') do
      table.insert(list, expect_expression(L))
    end
  end
  return list
end

-- Tries to parse: [ <argument-declaration-list> ]
-- Returns the AST object for a argument declaration list
-- Returns an empty list if no declarations could be read
local function parse_argument_declarations(L)
  local list = {}
  local type_spec = parse_type_specifier(L)
  if type_spec then
    local name = expect_name(L)
    table.insert(list, { type_specifier = type_spec, name = name })
    while parse_token(L, 'comma') do
      local type_spec = expect_type_specifier(L)
      local name = expect_name(L)
      table.insert(list, { type_specifier = type_spec, name = name })
    end
  end
  return list
end

-- Tries to parse the rule: <expression-p0>
-- Returns the AST object for an expression
local function parse_expression_p0(L)
  if L.next.type == 'lparen' then
    L:read()
    local subexpr = parse_expression(L)
    expect_token(L, 'rparen')
    return subexpr
  elseif L.next.type == 'minus' then
    L:read()
    local subexpr = parse_expression_p0(L)
    return ast.expr_negate(subexpr)
  elseif L.next.type == 'tilde' then
    L:read()
    local subexpr = parse_expression_p0(L)
    return ast.expr_binnot(subexpr)
  elseif L.next.type == 'lognot' then
    L:read()
    local subexpr = parse_expression_p0(L)
    return ast.expr_lognot(subexpr)
  elseif L.next.type == 'integer' then
    local value = L.next.value
    L:read()
    return ast.expr_integer(value)
  else
    -- <name-path> [ '(' [ <arguments> ] ')' ]
    local st = L:state()
    local name_path = parse_name_path(L)
    if name_path then
      if parse_token(L, 'lparen') then
        -- Function call
        local args = parse_argument_list(L)
        expect_token(L, 'rparen')
        return ast.expr_call(name_path, args)
      end
    end
    -- <lvalue>
    L:reset(st)
    local lvalue = parse_lvalue(L)
    if lvalue then
      return ast.expr_lvalue(lvalue)
    end
  end
end

-- Tries to parse the rule: <expression-p1>
-- Returns the AST object for an expression
local function parse_expression_p1(L)
  -- Read first operator argument (may be the only one)
  local subexpr = parse_expression_p0(L)
  while L.next.type == 'asterisk' or L.next.type == 'fslash' do
    -- Read operator
    local optype = L.next.type
    L:read()
    -- Read second operator argument
    local subexpr2 = parse_expression_p0(L)
    if optype == 'asterisk' then
      subexpr = ast.expr_mul(subexpr, subexpr2)
    elseif optype == 'fslash' then
      subexpr = ast.expr_div(subexpr, subexpr2)
    end
  end
  return subexpr
end

-- Tries to parse the rule: <expression-p2>
-- Returns the AST object for an expression
local function parse_expression_p2(L)
  -- Read first operator argument (may be the only one)
  local subexpr = parse_expression_p1(L)
  while L.next.type == 'plus' or L.next.type == 'minus' do
    -- Read operator
    local optype = L.next.type
    L:read()
    -- Read second operator argument
    local subexpr2 = parse_expression_p1(L)
    if optype == 'plus' then
      subexpr = ast.expr_add(subexpr, subexpr2)
    elseif optype == 'minus' then
      subexpr = ast.expr_sub(subexpr, subexpr2)
    end
  end
  return subexpr
end

-- Tries to parse the rule: <expression-p3>
-- Returns the AST object for an expression
local function parse_expression_p3(L)
  -- Read first operator argument (may be the only one)
  local subexpr = parse_expression_p2(L)
  while L.next.type == 'cmplt' or
        L.next.type == 'cmpgt' or
        L.next.type == 'cmpleq' or
        L.next.type == 'cmpgeq' do
    -- Read operator
    local optype = L.next.type
    L:read()
    -- Read second operator argument
    local subexpr2 = parse_expression_p2(L)
    if optype == 'cmplt' then
      subexpr = ast.expr_cmplt(subexpr, subexpr2)
    elseif optype == 'cmpgt' then
      subexpr = ast.expr_cmpgt(subexpr, subexpr2)
    elseif optype == 'cmpleq' then
      subexpr = ast.expr_cmpleq(subexpr, subexpr2)
    elseif optype == 'cmpgeq' then
      subexpr = ast.expr_cmpgeq(subexpr, subexpr2)
    end
  end
  return subexpr
end

-- Tries to parse the rule: <expression-p4>
-- Returns the AST object for an expression
local function parse_expression_p4(L)
  -- Read first operator argument (may be the only one)
  local subexpr = parse_expression_p3(L)
  while L.next.type == 'cmpeq' or L.next.type == 'cmpneq' do
    -- Read operator
    local optype = L.next.type
    L:read()
    -- Read second operator argument
    local subexpr2 = parse_expression_p3(L)
    if optype == 'cmpeq' then
      subexpr = ast.expr_cmpeq(subexpr, subexpr2)
    elseif optype == 'cmpneq' then
      subexpr = ast.expr_cmpneq(subexpr, subexpr2)
    end
  end
  return subexpr
end

-- Tries to parse the rule: <expression-p5>
-- Returns the AST object for an expression
local function parse_expression_p5(L)
  -- Read first operator argument (may be the only one)
  local subexpr = parse_expression_p4(L)
  while L.next.type == 'logand' do
    -- Read operator
    local optype = L.next.type
    L:read()
    -- Read second operator argument
    local subexpr2 = parse_expression_p4(L)
    if optype == 'logand' then
      subexpr = ast.expr_logand(subexpr, subexpr2)
    end
  end
  return subexpr
end

-- Tries to parse the rule: <expression-p6>
-- Returns the AST object for an expression
local function parse_expression_p6(L)
  -- Read first operator argument (may be the only one)
  local subexpr = parse_expression_p5(L)
  while L.next.type == 'logor' do
    -- Read operator
    local optype = L.next.type
    L:read()
    -- Read second operator argument
    local subexpr2 = parse_expression_p5(L)
    if optype == 'logor' then
      subexpr = ast.expr_logor(subexpr, subexpr2)
    end
  end
  return subexpr
end

-- Tries to parse the rule: <expression>
parse_expression = parse_expression_p6

-- Expects to parse the rule: <expression>
-- Returns the AST object for an expression
function expect_expression(L)
  local expr = parse_expression(L)
  if not expr then
    parse_abort_expected(L, 'expression')
  end
  return expr
end

-- Tries to parse the rule: <block>
-- Returns the AST object for a block statement
local function parse_block(L)
  if parse_token(L, 'lcurly') then
    local block = ast.stmt_block()
    while not parse_token(L, 'rcurly') do
      table.insert(block, expect_statement(L))
    end
    return block
  end
end

-- Expects to parse the rule: <block>
-- Returns the AST object for a block statement
local function expect_block(L)
  local block = parse_block(L)
  if not block then
    parse_abort_expected(L, 'block')
  end
  return block
end

-- Attempts to parse the rule: <if-body>
-- Returns the expression AST object and the block AST object separately
local function parse_if_body(L)
  local expr, block
  if parse_token(L, 'if') then
    expect_token(L, 'lparen')
    expr = expect_expression(L)
    expect_token(L, 'rparen')
    block = expect_block(L)
    return expr, block
  end
end

-- Tries to parse the rule: <else-body>
-- Returns the AST object for an if statement
local function parse_else_body(L)
  if parse_token(L, 'else') then
    local stmt = parse_if_statement(L)
    if stmt then return stmt end
    stmt = parse_block(L)
    if stmt then return stmt end
    -- Curly braces are not optional!
    parse_abort_expected(L, 'if', 'lcurly')
  end
end

-- Tries to parse the rule: <if-statement>
-- Returns the AST object for an if statement
function parse_if_statement(L)
  local expr, if_stmt = parse_if_body(L)
  if expr then
    -- if statement parsed, try to read its else clause
    local else_stmt = parse_else_body(L)
    return ast.stmt_if(expr, if_stmt, else_stmt)
  end
end

-- Tries to parse the rule: <function-call>
-- Returns the AST object for a function call statement
local function parse_function_call(L)
  local st = L:state()
  local name_path = parse_name_path(L)
  if name_path then
    -- Try to read an lparen
    if parse_token(L, 'lparen') then
      -- Read function call argument list
      local args = parse_argument_list(L)
      expect_token(L, 'rparen')
      expect_token(L, 'semicolon')
      return ast.stmt_function_call(name_path, args)
    end
  end
  L:reset(st)
end

-- Tries to parse the rule: <lvalue-decl>
-- Returns the AST object for an lvalue
local function parse_lvalue_decl(L)
  -- <type-specifier> <name>
  local st = L:state()
  local type_spec = parse_type_specifier(L)
  if type_spec then
    local name = parse_name(L)
    if name then
      return ast.lvalue_declaration(type_spec, name)
    end
  end
  -- <lvalue>
  L:reset(st)
  return parse_lvalue(L)
end

-- Expects to parse the rule: <lvalue-decl>
-- Returns the AST object for an lvalue
local function expect_lvalue_decl(L)
  local lvalue = parse_lvalue_decl(L)
  if not lvalue then
    parse_abort_expected(L, 'lvalue')
  end
  return lvalue
end

-- Tries to parse the rule: <assignment>
-- Returns the AST object for an assignment statement
local function parse_assignment(L)
  local lvalue_list = {}
  local expr_list = {}
  -- Assignment begins with an lvalue
  local lvalue = parse_lvalue_decl(L)
  if not lvalue then return end
  table.insert(lvalue_list, lvalue)
  -- Keep reading lvalues if given
  while parse_token(L, 'comma') do
    lvalue = expect_lvalue_decl(L)
    table.insert(lvalue_list, lvalue)
  end
  -- True assigment requires an equals sign, but local variable declarations need not have one
  if parse_token(L, 'equals') then
    local expr = expect_expression(L)
    table.insert(expr_list, expr)
    -- Keep reading expressions if given
    while parse_token(L, 'comma') do
      expr = expect_expression(L)
      table.insert(expr_list, expr)
    end
  end
  -- And that's the statement
  expect_token(L, 'semicolon');
  return ast.stmt_assignment(lvalue_list, expr_list)
end

-- Tries to parse the rule: <return-statement>
-- Returns the AST object for a return statement
local function parse_return_statement(L)
  if parse_token(L, 'return') then
    local args = parse_argument_list(L)
    local stmt = ast.stmt_return(args)
    expect_token(L, 'semicolon')
    return stmt
  end
end

-- Expects to parse the rule: <statement>
-- Returns the AST object for a statement
function expect_statement(L)
  local stmt
  -- <return-statement>
  stmt = parse_return_statement(L)
  if stmt then return stmt end
  -- <block>
  stmt = parse_block(L)
  if stmt then return stmt end
  -- <if-statement>
  stmt = parse_if_statement(L)
  if stmt then return stmt end
  -- <function-call>
  stmt = parse_function_call(L)
  if stmt then return stmt end
  -- <assignment>
  stmt = parse_assignment(L)
  if stmt then return stmt end
  -- :c
  parse_abort_expected(L, 'statement')
end

-- Tries to parse the rule: <function-declaration>
-- Returns the AST object for a function declaration
local function parse_function_declaration(L)
  if parse_token(L, 'function') then
    local name = expect_name(L)
    expect_token(L, 'lparen')
    local arguments = parse_argument_declarations(L)
    local returns = { }
    expect_token(L, 'rparen')
    if parse_token(L, 'rarrow') then
      expect_token(L, 'lparen');
      returns = parse_argument_declarations(L)
      expect_token(L, 'rparen')
    end
    local block = parse_block(L)
    return ast.function_declaration(name, arguments, returns, block)
  end
end

-- Tries to parse the rule: <member-declaration>
-- Returns the AST object for a member declaration
local function parse_member_declaration(L)
  local type_name_path = parse_name_path(L)
  if type_name_path then
    local name = expect_name(L)
    expect_token(L, 'semicolon')
    return ast.member_declaration(type_name_path, name)
  end
end

-- Expects to parse the rule: <module-body-declaration>
-- Returns the appropriate AST object for the declaration
local function expect_module_body_declaration(L)
  local decl = parse_function_declaration(L)
  if decl then return decl end
  decl = parse_member_declaration(L)
  if decl then return decl end
  parse_abort_expected(L, 'member/function declaration');
end

-- Tries to parse the rule: <module-declaration>
-- Returns the AST object for a module declaration
local function parse_module_declaration(L)
  local name_path 
  local declarations = {}
  if parse_token(L, 'module') then
    name_path = expect_name_path(L)
    expect_token(L, 'lcurly')
    -- Read declarations until an rcurly
    while not parse_token(L, 'rcurly') do
      table.insert(declarations, expect_module_body_declaration(L))
    end
    return ast.module_declaration(name_path, declarations)
  end
end

-- Expects to parse the rule: <file-scope-declaration>
-- Returns the appropriate AST object for the declaration
local function expect_file_scope_declaration(L)
  local decl = parse_struct_declaration(L)
  if decl then return decl end
  decl = parse_module_declaration(L)
  if decl then return decl end
  parse_abort_expected(L, 'file-scope declaration');
end

-- Parses a file
-- Returns the AST object for a translation unit
return function(file_in)
  local L = Lexer(file_in)
  local file_declarations = {}
  while not parse_token(L, 'eof') do
    table.insert(file_declarations, expect_file_scope_declaration(L))
  end
  return file_declarations
end

