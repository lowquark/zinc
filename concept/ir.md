
##Stuff:

Arguments and returns are already on the stack prior to a function call, so they are included as part of the
subroutine declaration.

All registers are word-sized; they may be of type int or addr.

    ; Returns the sum of the arguments multiplied by the first argument.
    sub func1(a0 : int, a1 : int ; r0 : int)
      local l0
      l0 := a0 + a1
      r0 := a0 * l0
      ret

    ; If a0 points to an array of 64-bit integers, func2 returns the element at index 1.
    sub func2(a0 : addr ; r0 : int)
      s0 := a0 + 8
      r0 := [s0]
      ret

##Input:

    function func1(int a0, int a1) -> int {
      return a0 * (a0 + a1)
    }

##AST:

    FUNCTION func1 ARGUMENTS(a0 : int, a1 : int) RETURNS(r0 : int)
      BLOCK
        RETURN
          MUL
            VARIABLE a0
            ADD
              VARIABLE a0
              VARIABLE a1

##IR:

    sub func1(a0 : int, a1 : int ; r0 : int)
      local l0
      l0 := a0 + a1
      r0 := a0 * l0
      ret

