
----------------------------------------------------------------------------------------------------
-- Parse a file and generate an AST
----------------------------------------------------------------------------------------------------

local ast = require 'ast'

----------------------------------------------------------------------------------------------------
-- Lexer

local function Lexer(file)
  local L = { line = 1, col = 1 }

  local function print_token(tok)
    if tok.value then
      io.write('type: `'..tok.type..'` value: `'..tok.value..'`\n')
    else
      io.write('type: `'..tok.type..'`\n')
    end
  end

  local function yield(token)
    L.next = token
    -- Uncomment to print every token encountered
    --print_token(token)
    coroutine.yield(token)
  end

  local function readc()
    L.col = L.col + 1
    return file:read(1)
  end

  setmetatable(L, { __index = {
    read = coroutine.wrap(function(self)
      local c = readc()
      while true do
        if not c then
          yield { type = 'eof' }
        elseif string.match(c, '[%a_]') then
          -- This is a name or reserved keyword
          str = c
          c = readc()
          -- Keep matching, allowing decimals now
          while string.match(c, '[%w_]') do
            str = str .. c
            c = readc()
          end
          if str == 'access' then
            yield { type = 'access' }
          elseif str == 'module' then
            yield { type = 'module' }
          elseif str == 'struct' then
            yield { type = 'struct' }
          elseif str == 'function' then
            yield { type = 'function' }
          elseif str == 'return' then
            yield { type = 'return' }
          elseif str == 'if' then
            yield { type = 'if' }
          elseif str == 'else' then
            yield { type = 'else' }
          else
            yield { type = 'name', value = str }
          end
        elseif string.match(c, '%d') then
          -- This is a number
          str = c
          c = readc()
          -- Keep matching
          while string.match(c, '[%d]') do
            str = str .. c
            c = readc()
          end
          yield { type = 'integer', value = str }
        elseif c == '{' then
          c = readc()
          yield { type = 'lcurly' }
        elseif c == '}' then
          c = readc()
          yield { type = 'rcurly' }
        elseif c == '(' then
          c = readc()
          yield { type = 'lparen' }
        elseif c == ')' then
          c = readc()
          yield { type = 'rparen' }
        elseif c == ';' then
          c = readc()
          yield { type = 'semicolon' }
        elseif c == ':' then
          c = readc()
          yield { type = 'colon' }
        elseif c == '.' then
          c = readc()
          yield { type = 'dot' }
        elseif c == ',' then
          c = readc()
          yield { type = 'comma' }
        elseif c == '+' then
          c = readc()
          yield { type = 'plus' }
        elseif c == '-' then
          c = readc()
          if c == '>' then
            c = readc()
            yield { type = 'rarrow' }
          else
            yield { type = 'minus' }
          end
        elseif c == '*' then
          c = readc()
          yield { type = 'asterisk' }
        elseif c == '/' then
          c = readc()
          yield { type = 'fslash' }
        elseif c == '~' then
          c = readc()
          yield { type = 'tilde' }
        elseif c == '!' then
          c = readc()
          if c == '=' then
            c = readc()
            yield { type = 'cmpneq' }
          else
            yield { type = 'lognot' }
          end
        elseif c == '<' then
          c = readc()
          if c == '=' then
            c = readc()
            yield { type = 'cmpleq' }
          else
            yield { type = 'cmplt' }
          end
        elseif c == '>' then
          c = readc()
          if c == '=' then
            c = readc()
            yield { type = 'cmpgeq' }
          else
            yield { type = 'cmpgt' }
          end
        elseif c == '&' then
          c = readc()
          if c == '&' then
            c = readc()
            yield { type = 'logand' }
          else
            yield { type = 'binand' }
          end
        elseif c == '|' then
          c = readc()
          if c == '|' then
            c = readc()
            yield { type = 'logor' }
          else
            yield { type = 'binor' }
          end
        elseif c == '=' then
          c = readc()
          if c == '=' then
            c = readc()
            yield { type = 'cmpeq' }
          else
            yield { type = 'equals' }
          end
        elseif c == '\n' then
          c = readc()
          self.line = self.line + 1
          self.col = 0
        else
          c = readc()
        end
      end
      file:close()
    end),
    close = function(self)
      file:close()
    end
  }})

  L:read()

  return L
end

----------------------------------------------------------------------------------------------------
-- Parser

--[[

Full parser grammar, in LL(1) extended BNF:

<name-path-rest> := { ':' <name> }
<name-path> := <name> <name-path-rest>

<expression-p0> := '(' <expression> ')'
                 | ( '-' | '~' | '!' ) <expression-p0>
                 | <integer>
                 | <name-path> [ '(' [ <arguments> ] ')' ]
<expression-p1> := <expression-p0> { ( '*' | '/' ) <expression-p0> }
<expression-p2> := <expression-p1> { ( '+' | '-' ) <expression-p1> }
<expression-p3> := <expression-p2> { ( '<' | '>' | '<=' | '>=' ) <expression-p2> }
<expression-p4> := <expression-p3> { ( '==' | '!=' ) <expression-p3> }
<expression-p5> := <expression-p4> { '&&' <expression-p4> }
<expression-p6> := <expression-p5> { '||' <expression-p5> }
<expression> := <expression-p6>

<arguments> := <expression> { ',' <expression> }
<argument-declarations> := <name-path> <name> { ',' <name-path> <name> }

<if-body> := 'if' '(' <expression> ')' <block>
<else-body> := 'else' ( <if-body> [ <else-body> ] | <block> )
<if-statement> := <if-body> [ <else-body> ]

<statement-post-name-path> := <name> ';' | '(' [ <arguments> ] ')' ';'
<statement-post-name> := '=' <expression> ';' | <name-path-rest> <statement-post-name-path>
<statement> := 'return' [ <expression> ] ';'
             | <if-statement>
             | <block>
             | <name> <statement-post-name>
             | <expression> ';'

<block> := '{' { <statement> } '}'

<function-declaration> := 'function' <name> '(' [ <argument-declarations> ] ')'
                          [ '->' '(' [ <argument-declarations> ] ')' ]
                          <block>
<member-declaration> := <name-path> <name> ';'
<module-body-declaration> := <function-declaration> | <member-declaration>
<module-declaration> := 'module' <name-path> '{' { <module-body-declaration> } '}'

<access-declaration> := 'access' <name-path> ';'
<field-declaration> := <name-path> <name> ';'
<struct-body-declaration> := <access-declaration> | <field-declaration>
<struct-declaration> := 'struct' <name-path> '{' { <struct-body-declaration> } '}'

<file-scope-declaration> := <struct-declaration> | <module-declaration>

]]

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

local parse_expression
local expect_expression

-- Tries to parse: [ <argument-list> ]
-- Returns the AST object for an argument list
-- Returns an empty list if no expressions could be read
local function parse_arguments(L)
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
  local name_path = parse_name_path(L)
  if name_path then
    local name = expect_name(L)
    table.insert(list, { type_name_path = name_path, name = name })
    while parse_token(L, 'comma') do
      local name_path = expect_name_path(L)
      local name = expect_name(L)
      table.insert(list, { type_name_path = name_path, name = name })
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
    local name_path = parse_name_path(L)
    if name_path then
      local args
      -- This might be a function call
      if parse_token(L, 'lparen') then
        args = parse_arguments(L)
        expect_token(L, 'rparen')
      end

      if args then
        return ast.expr_call(name_path, args)
      else
        if #name_path == 1 then
          local name = name_path[1]
          return ast.expr_variable(name)
        else
          error('Invalid variable reference')
        end
      end
    else
      return nil
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
  while L.next.type == 'cmplt' or L.next.type == 'cmpgt' or L.next.type == 'cmpleq' or L.next.type == 'cmpgeq' do
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
expect_expression = function(L)
  local expr = parse_expression(L)
  if not expr then
    parse_abort_expected(L, 'expression')
  end
  return expr
end

local expect_statement
local parse_if_statement

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
parse_if_statement = function(L)
  local expr, if_stmt = parse_if_body(L)
  if expr then
    -- if statement parsed, try to read its else clause
    local else_stmt = parse_else_body(L)
    return ast.stmt_if(expr, if_stmt, else_stmt)
  end
end

-- Expects to parse the rule: <statement>
-- Returns the AST object for a statement
expect_statement = function(L)
  local stmt
  if parse_token(L, 'return') then
    -- Easiest statement ever
    stmt = ast.stmt_return(parse_expression(L))
    expect_token(L, 'semicolon')
    return stmt
  end
  stmt = parse_if_statement(L)
  if stmt then return stmt end
  stmt = parse_block(L)
  if stmt then return stmt end
  local first_name = parse_name(L)
  if first_name then
    -- Reading a name at the beginning of a statement is pretty ambiguous
    if parse_token(L, 'equals') then
      -- Assigning first_name from expr
      local expr = expect_expression(L)
      expect_token(L, 'semicolon')
      -- That's a variable assignment
      return ast.stmt_assign(first_name, expr)
    end
    -- Read remainder of path, if applicable (first name already known)
    name_path = parse_name_path_rest(L, first_name)
    -- Try to read a name
    local name = parse_name(L)
    if name then
      -- name_path is a local variable declaration's type
      expect_token(L, 'semicolon')
      -- That's the declaration, me boy
      return ast.stmt_local_declaration(name_path, name)
    end
    -- Try to read an lparen
    if parse_token(L, 'lparen') then
      -- Read function call argument list
      local args = parse_arguments(L)
      expect_token(L, 'rparen')
      expect_token(L, 'semicolon')
      return ast.stmt_function_call(name_path, args)
    end
    parse_abort_expected(L, 'equals', 'colon', 'name', 'lparen')
  end
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
  L:close()
  return file_declarations
end

