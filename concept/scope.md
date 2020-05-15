
## Scoping and variable allocation

Consider 4 stack-allocated variables, `S0`, `S1`, `S2`, and `S3`. Assume they are allocated as
follows:

           S0              S1          S2      S3              S0 :  0, 8
    +---------------+---------------+-------+-------+          S1 :  8, 8
    |      (8)      |      (8)      |  (4)  |  (4)  |          S2 : 16, 4
    +---------------+---------------+-------+-------+          S3 : 20, 4

It's easy to see that all of these variables are candidates for register storage. Consider now
giving them overlapping addresses, as one would through block scoping:

           S0              S1
    +---------------+---------------+          S0 :  0, 8
    |      (8)      |      (8)      |          S1 :  8, 8
    +---------------+---------------+          S2 :  8, 4
                    |  (4)  |  (4)  |          S3 : 12, 4
                    +-------+-------+
                       S2      S3

Scope-wise, this would correspond to:

    block {
      i64 a;
      block {
        int64 b;
      }
      block {
        int32 c;
        int32 d;
      }
    }

Where `a` corresponds to `S0`, `b` to `S1`, `c` to `S2`, and `d` to `S3`. Somehow all of these
variables should remain candidates for register allocation, despite now having overlapping
addresses.

## Idea

Basic stack allocation ensures that the variables in a given scope do not overlap in memory. This
trivally ensures that the in-scope stack variables are alias-free. Additionally, the language's
scoping rules (essentially those of C) ensure something similar for the variables which do alias:
because they are in different scopes, the program will never write to one variable and expect to
read that value from another. Indeed, these variables are said to have gone _out-of-scope_.

Essentially, we're allowed to move an arbitrary variable from the stack to a register, if we know no
other code depends on its value being in that address. This is always true, except for those cases
where an address is taken, and for members of of union-types (which I would love to use for
casting). As long as the address of a stack-allocated, non-union-member is not required, we can
store it in a register.

## New Procedure

 1. Compile
      - Always allocate temporaries and variables on the stack ('s' registers)
      - Assign each variable a type, offset and size
      - Record variables whose address is required
 2. Allocate Registers
      - Consider variables of basic types whose address is not required
      - Allocate by placing in 'r' register
      - Spill to stack by keeping original 's' register
 3. Generate
      - Map 'r' registers to machine register expressions (e.g.: `%rbx`)
      - Map 's' registers to memory expressions (e.g.: `-40(%rbp)`)

