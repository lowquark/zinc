
----------------------------------------------------------------------------------------------------
-- Lexer

local pprint = require 'pprint'

local function token(type, value)
  local tok = { type = type, value = value }
  return tok
end

local function print_token(tok)
  if tok.value then
    io.write('type: `'..tok.type..'` value: `'..tostring(tok.value)..'`\n')
  else
    io.write('type: `'..tok.type..'`\n')
  end
end

local function yield(L, token)
  L.next = token
  print_token(token)
  return token
end

local function readc(L)
  local c = L.file:read(1)
  L.col = L.col + 1
  L.c = c
  return c
end

local keyword_set = {
  ['access'] = true,
  ['module'] = true,
  ['struct'] = true,
  ['function'] = true,
  ['return'] = true,
  ['if'] = true,
  ['else'] = true,
}

local function is_keyword(str)
  return keyword_set[str] or false
end

local function read(L)
  while true do
    local c = L.c
    if not c then
      return yield(L, token 'eof')
    elseif string.match(c, '[%a_]') then
      -- This is a name or reserved keyword
      str = c
      c = readc(L)
      -- Keep matching, allowing decimals now
      while string.match(c, '[%w_]') do
        str = str .. c
        c = readc(L)
      end
      if is_keyword(str) then
        return yield(L, token(str))
      else
        return yield(L, token('name', str))
      end
    elseif string.match(c, '%d') then
      -- This is a number
      str = c
      c = readc(L)
      -- Keep matching
      while string.match(c, '[%d]') do
        str = str .. c
        c = readc(L)
      end
      return yield(L, token('integer', str))
    elseif c == '{' then
      c = readc(L)
      return yield(L, token 'lcurly')
    elseif c == '}' then
      c = readc(L)
      return yield(L, token 'rcurly')
    elseif c == '(' then
      c = readc(L)
      return yield(L, token 'lparen')
    elseif c == ')' then
      c = readc(L)
      return yield(L, token 'rparen')
    elseif c == ';' then
      c = readc(L)
      return yield(L, token 'semicolon')
    elseif c == ':' then
      c = readc(L)
      return yield(L, token 'colon')
    elseif c == '.' then
      c = readc(L)
      return yield(L, token 'dot')
    elseif c == ',' then
      c = readc(L)
      return yield(L, token 'comma')
    elseif c == '+' then
      c = readc(L)
      return yield(L, token 'plus')
    elseif c == '-' then
      c = readc(L)
      if c == '>' then
        c = readc(L)
        return yield(L, token 'rarrow')
      else
        return yield(L, token 'minus')
      end
    elseif c == '*' then
      c = readc(L)
      return yield(L, token 'asterisk')
    elseif c == '/' then
      c = readc(L)
      return yield(L, token 'fslash')
    elseif c == '~' then
      c = readc(L)
      return yield(L, token 'tilde')
    elseif c == '!' then
      c = readc(L)
      if c == '=' then
        c = readc(L)
        return yield(L, token 'cmpneq')
      else
        return yield(L, token 'lognot')
      end
    elseif c == '<' then
      c = readc(L)
      if c == '=' then
        c = readc(L)
        return yield(L, token 'cmpleq')
      else
        return yield(L, token 'cmplt')
      end
    elseif c == '>' then
      c = readc(L)
      if c == '=' then
        c = readc(L)
        return yield(L, token 'cmpgeq')
      else
        return yield(L, token 'cmpgt')
      end
    elseif c == '&' then
      c = readc(L)
      if c == '&' then
        c = readc(L)
        return yield(L, token 'logand')
      else
        return yield(L, token 'binand')
      end
    elseif c == '|' then
      c = readc(L)
      if c == '|' then
        c = readc(L)
        return yield(L, token 'logor')
      else
        return yield(L, token 'binor')
      end
    elseif c == '=' then
      c = readc(L)
      if c == '=' then
        c = readc(L)
        return yield(L, token 'cmpeq')
      else
        return yield(L, token 'equals')
      end
    elseif c == '\n' then
      c = readc(L)
      L.line = L.line + 1
      L.col = 0
    else
      c = readc(L)
    end
  end
end

local function push(L)
  local st = { line = L.line, col = L.col, c = L.c, next = L.next, seek = L.file:seek() }
  table.insert(L.states, st)
end

local function pop(L)
  local st = table.remove(L.states)
  L.line = st.line
  L.col = st.col
  L.c = st.c
  L.next = st.next
  L.file:seek('set', st.seek)
end

local function Lexer(file)
  local L = { file = file, line = 1, col = 1, states = {} }

  setmetatable(L, { __index = {
    read = read,
    push = push,
    pop = pop,
  }})

  readc(L)
  L:read()
  pprint(L)

  return L
end

return Lexer

