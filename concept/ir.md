
# Intermediate Representation

This document briefly describes the intermediate representation (IR) used by the zinc compiler.

## Program

The abstract syntax tree (AST) of each zinc program is analyzed, and converted into an IR program.
An IR program consists of a list of IR subroutines and a list of global variables.

## Subroutine

Each IR subroutine defines its name, argument types, and list of instructions. For example:

    sub func1(int,int : int)
      ret

Defines a subroutine named `func1` which takes two int parameters and returns a single int. The only
instruction in the subroutine body is a return instruction, so it will do nothing with its inputs
and simply return.

## Registers

4 types of registers are available for use with within instructions: temporary, stack, input, and
argument. Any register type may be used as an instruction operand or destination, provided the
register is being used in the appropriate context.

### Temporary Registers

Each subroutine has at its disposal an unlimited number of __temporary registers__.  Temporary
register names start with an 'r' and are followed by a non-negative integer, e.g.: r0, r1, r2, etc.
Temporary registers have no context restrictions and may be used by the subroutine at any point.

### Stack Registers

Additionally, subroutines have access to __stack registers__, registers which are assumed to reside
in memory. Stack register names start with an 's' and are similarly followed by a non-negative
integer which corresponds to their address on the stack.

Before using a stack register, the subroutine must first allocate stack space. This is accomplished
through the salloc and sfree instructions. In the following snippet, stack space is allocated for
stack registers 0-3, and is freed before the end of the function.

    sub func1( : )
      salloc 4
      s0 := 1
      s1 := 2
      s2 := 3
      s3 := 4
      sfree 4
      ret

Stack space is cumulative, so the following snippet is functionally equivalent to the previous:

    sub func1( : )
      salloc 2
      s0 := 1
      s1 := 2
      salloc 2
      s2 := 3
      s3 := 4
      sfree 2
      sfree 2
      ret

### Input Registers

When a subroutine is called with arguments, those arguments are present in a block of registers
known as __input registers__. The block is shared by both arguments and return values of the
subroutine, so there are as many input registers as there are arguments or return values, whichever
requires more. Input register names start with an 'i' and are followed by a non-negative integer
indicating their argument/return value index.

The following snippet returns the sum of its arguments in its first return value, and the difference
of its arguments in the second. In a certain sense, it performs a transformation on its input data.

    sub sum_and_diff(int,int : int,int)
      r0 := i0
      r1 := i1
      i0 := r0 + r1
      i1 := r0 - r1
      ret

### Argument Registers

In order to exchange arguments and return values with a subroutine, a block of __argument
registers__ is allocated by the calling function. The caller populates the argument registers with
the subroutine arguments, executes the subroutine, and reads the results from the same registers.
Argument register names start with an 'a' and are followed by a non-negative integer, indicating
their argument/return value index. These indices correspond direcly to the input register indices of
the called function, i.e.: a0 &#x21D4; i0, a1 &#x21D4; i1, etc.

The following snippet calls the function of the previous example, and stores its results in
temporary registers before returning.

    sub func1(int,int : int,int)
      begincall 2
      a0 := 5
      a1 := 5
      call sum_and_diff
      r0 := a0
      r1 := a1
      endcall
      ret

The use of argument registers is only valid after a begincall instruction, and before its
corresponding endcall instruction. An argument register always corresponds to the argument register
of the current begincall instruction.

