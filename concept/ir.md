
# Intermediate Representation

This document briefly describes the intermediate representation (IR) used by the zinc compiler.

## Program

The abstract syntax tree (AST) of each zinc program is analyzed, and converted into an IR program.
An IR program consists of a list of IR subroutines and a list of global variables.

## Subroutine

Each IR subroutine defines its name, argument types, and list of instructions. For example:

    sub func1(int64,int64 : int64)
      ret

Defines a subroutine named `func1` which takes two int64 parameters and returns a single int64. The
only instruction in the subroutine body is a return instruction, so it will do nothing with its
inputs and simply return.

## Registers

3 types of registers are available for use with within instructions: temporary, stack, and argument.
Any register type may be used as an instruction operand or destination, provided the register is
being used in the appropriate context.

### Temporary Registers

Each subroutine has at its disposal an unlimited number of __temporary registers__. Temporary
register names start with an 'r' and are followed by a non-negative integer, e.g.: r0, r1, r2, etc.
Temporary registers have no context restrictions and may be used by the subroutine at any point.

### Stack Registers

Additionally, subroutines have access to an unlimited number of __stack registers__, values which
are assumed to reside in memory. Stack register names start with an 's' and are similarly followed
by a non-negative integer which corresponds to their address on the stack.

### Argument Registers

When a subroutine is called with arguments, the argument values are present in a finite block of
registers known as __argument registers__. The block is shared by both the arguments and return
values of the subroutine, so there are as many input registers as there are arguments or return
values, whichever requires more. Input register names start with an 'i' and are followed by a
non-negative integer indicating their argument/return value index.

The following snippet returns the sum of its arguments in its first return value, and the difference
of its arguments in the second. In a certain sense, it performs a transformation on its input data.

    sub sum_and_diff(int,int : int,int)
      r0 := i0
      r1 := i1
      i0 := r0 + r1
      i1 := r0 - r1
      ret

