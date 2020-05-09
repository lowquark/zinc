
Full parser grammar, in extended BNF:

    <name-path-rest> := { ':' <name> }
    <name-path> := <name> <name-path-rest>

    <expression-p0> := '(' <expression> ')'
                     | ( '-' | '~' | '!' ) <expression-p0>
                     | <integer>
                     | <name-path> [ '[' <integer> ']' ] [ '(' [ <arguments> ] ')' ]
    <expression-p1> := <expression-p0> { ( '*' | '/' ) <expression-p0> }
    <expression-p2> := <expression-p1> { ( '+' | '-' ) <expression-p1> }
    <expression-p3> := <expression-p2> { ( '<' | '>' | '<=' | '>=' ) <expression-p2> }
    <expression-p4> := <expression-p3> { ( '==' | '!=' ) <expression-p3> }
    <expression-p5> := <expression-p4> { '&&' <expression-p4> }
    <expression-p6> := <expression-p5> { '||' <expression-p5> }
    <expression> := <expression-p6>

    <type-specifier> := [ 'const' ] <name-path> [ '[' [ <integer> ] ']' ] [ '&' ]

    <arguments> := <expression> { ',' <expression> }
    <argument-declarations> := <type-specifier> <name> { ',' <type-specifier> <name> }

    <return-statement> := 'return' [ <arguments> ] ';'

    <block> := '{' { <statement> } '}'

    <if-body> := 'if' '(' <expression> ')' <block>
    <else-body> := 'else' ( <if-body> [ <else-body> ] | <block> )
    <if-statement> := <if-body> [ <else-body> ]

    <function-call> := <name-path> '(' [ <arguments> ] ')'

    <lvalue> := <type-specifier> <name> | <name-path> [ '[' <integer> ']' ]
    <assignment> := <lvalue> { ',' <lvalue> } [ '=' <expression> { ',' <expression> } ] ';'

    <statement> := <return-statement>
                 | <block>
                 | <if-statement>
                 | <function-call>
                 | <assignment>

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

