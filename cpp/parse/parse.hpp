#ifndef PARSE_HPP
#define PARSE_HPP

#include <ast.hpp>

#include <iostream>

namespace parse {
  ast::file parse(std::istream & is);
}

#endif
