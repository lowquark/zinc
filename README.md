
# Zinc

Zinc is a data-driven programming language that I have mapped out and partially brought to life.

### Features:

  * Namespaces
  * Declarations which are initialized by default
  * Multiple assignment / return values
  * Const correctness
  * Built-in data structures (list, map, etc.)
  * C++ style references (&)
  * Write-access restrictions

That last feature refers to a visibility scheme that I have dubbed `access`. Here's a code
demonstration:

    struct MyStruct {
      int a;
      access module_a;
    }

    module module_a {
      function foo() {
        // OK
        MyStruct my_struct;
        my_struct.a = 5;
      }
    }

    module module_b {
      function bar() {
        // Not OK! module_b does not have access to MyStruct.
        MyStruct my_struct;
        my_struct.a = 5;
      }
    }

Basically, the modification of a struct is restricted to certain modules. All other functions are
given read-only access. This is very similar to C++'s `friend`, but instead applies to namespaces
(modules). This enables information hiding in a very straightforward way, and it expresses certain
dataflow patterns very simply and explicitly in a way that I think traditional OOP solutions do not.

# znc (Zinc Compiler)

I have a few thousand lines of Lua that spit out x86-64 assembly. One day I would like to have a C
implementation. It's not much yet, so here's some compiler output:

### Input file

    module my_module {
      int64 var_a;
      int64 var_b;

      function bla(int64 a, int64 b, int64 c) -> (int64 d, int64 e) {
        return a + b + c, a;
      }

      function main() -> (int64 ret) {
        int64 r = 3;
        int64 u = 2;
        int64 v = 4;
        int64 astro, v = my_module:bla(my_module:bla(r, r, r), u, v);
        int64 a, int64 b = 1, 0;
        a, b = b, a;
        astro = astro + b + v;
        return astro;
      }
    }

### Abstract syntax tree:

    MODULE my_module
      MEMBER var_a : int64
      MEMBER var_b : int64
      FUNCTION bla (int64 a, int64 b, int64 c) -> (int64 d, int64 e)
        BLOCK
          RETURN
            ADD 
              ADD 
                VARIABLE a
                VARIABLE b
              VARIABLE c
            VARIABLE a
      FUNCTION main () -> (int64 ret)
        BLOCK
          ASSIGNMENT
            DECLARATION int64 r
            :=
              INTEGER 3
          ASSIGNMENT
            DECLARATION int64 u
            :=
              INTEGER 2
          ASSIGNMENT
            DECLARATION int64 v
            :=
              INTEGER 4
          ASSIGNMENT
            DECLARATION int64 astro
            REFERENCE v
            :=
              CALL my_module:bla
                CALL my_module:bla
                  VARIABLE r
                  VARIABLE r
                  VARIABLE r
                VARIABLE u
                VARIABLE v
          ASSIGNMENT
            DECLARATION int64 a
            DECLARATION int64 b
            :=
              INTEGER 1
              INTEGER 0
          ASSIGNMENT
            REFERENCE a
            REFERENCE b
            :=
              VARIABLE b
              VARIABLE a
          ASSIGNMENT
            REFERENCE astro
            :=
              ADD 
                ADD 
                  VARIABLE astro
                  VARIABLE b
                VARIABLE v
          RETURN
            VARIABLE astro

### Compilation:

    i0 would be used in mov 2 after being assigned in mov 1
    local `r` is in register r0
    local `u` is in register r1
    local `v` is in register r2
    local `astro` is in register r3
    local `a` is in register r5
    local `b` is in register r6
    r5 would be used in mov 2 after being assigned in mov 1

### Intermediate representation:

    sub my_module$bla (int64,int64,int64 : int64,int64)
      r0 := i0 + i1
      r1 := r0 + i2
      r2 := i0
      i0 := r1
      i1 := r2
      ret
    sub my_module$main ( : int64)
      r0 := 3
      r1 := 2
      r2 := 4
      (r4, ~) := my_module$bla (r0, r0, r0)
      (r3, r2) := my_module$bla (r4, r1, r2)
      r5 := 1
      r6 := 0
      r7 := r5
      r5 := r6
      r6 := r7
      r8 := r3 + r6
      r9 := r8 + r2
      r3 := r9
      i0 := r3
      ret

### Liveness analysis and linear scan register allocation:

    r0 is live on [1, 2)
    r1 is live on [2, 4)
    r2 is live on [3, 5)
    map r0 -> r0
    map r1 -> r1
    map r2 -> r2
    sub my_module$bla (int64,int64,int64 : int64,int64)
      r0 := i0 + i1
      r1 := r0 + i2
      r2 := i0
      i0 := r1
      i1 := r2
      ret
    r0 is live on [1, 4)
    r1 is live on [2, 5)
    r2 is live on [3, 12)
    r4 is live on [4, 5)
    r3 is live on [5, 14)
    r5 is live on [6, 8)
    r6 is live on [7, 11)
    r7 is live on [8, 10)
    r8 is live on [11, 12)
    r9 is live on [12, 13)
    map r0 -> r0
    map r1 -> r1
    map r2 -> r2
    map r4 -> r3
    map r3 -> r4
    map r5 -> r5
    map r6 -> r0
    map r7 -> r3
    map r8 -> r1
    map r9 -> r5
    sub my_module$main ( : int64)
      r0 := 3
      r1 := 2
      r2 := 4
      (r3, ~) := my_module$bla (r0, r0, r0)
      (r4, r2) := my_module$bla (r3, r1, r2)
      r5 := 1
      r0 := 0
      r3 := r5
      r5 := r0
      r0 := r3
      r1 := r4 + r0
      r5 := r1 + r2
      r4 := r5
      i0 := r4
      ret

### Resulting assembly (AT&T Syntax):

            .text

            .globl my_module$bla
            .type my_module$bla, @function
    my_module$bla:
            push %rbp
            mov  %rsp, %rbp
            sub  $0, %rsp
            #  r0 := i0 + i1
            mov  32(%rbp), %rbx
            addq 24(%rbp), %rbx
            #  r1 := r0 + i2
            mov  %rbx, %rdx
            addq 16(%rbp), %rdx
            #  r2 := i0
            mov  32(%rbp), %r8
            #  i0 := r1
            mov  %rdx, 32(%rbp)
            #  i1 := r2
            mov  %r8, 24(%rbp)
            #  ret
            mov  %rbp, %rsp
            pop  %rbp
            ret

            .globl my_module$main
            .type my_module$main, @function
    my_module$main:
            push %rbp
            mov  %rsp, %rbp
            sub  $0, %rsp
            #  r0 := 3
            mov  $3, %rbx
            #  r1 := 2
            mov  $2, %rdx
            #  r2 := 4
            mov  $4, %r8
            #  (r3, ~) := my_module$bla (r0, r0, r0)
            push %rbx
            push %rdx
            push %r8
            push %r10
            push %r11
            sub  $24, %rsp
            mov  %rbx, -48(%rbp)
            mov  %rbx, -56(%rbp)
            mov  %rbx, -64(%rbp)
            call my_module$bla
            mov  -48(%rbp), %r9
            add  $24, %rsp
            pop %r11
            pop %r10
            pop %r8
            pop %rdx
            pop %rbx
            #  (r4, r2) := my_module$bla (r3, r1, r2)
            push %rbx
            push %rdx
            push %r9
            push %r11
            sub  $24, %rsp
            mov  %r9, -40(%rbp)
            mov  %rdx, -48(%rbp)
            mov  %r8, -56(%rbp)
            call my_module$bla
            mov  -40(%rbp), %r10
            mov  -48(%rbp), %r8
            add  $24, %rsp
            pop %r11
            pop %r9
            pop %rdx
            pop %rbx
            #  r5 := 1
            mov  $1, %r11
            #  r0 := 0
            mov  $0, %rbx
            #  r3 := r5
            mov  %r11, %r9
            #  r5 := r0
            mov  %rbx, %r11
            #  r0 := r3
            mov  %r9, %rbx
            #  r1 := r4 + r0
            mov  %r10, %rdx
            addq %rbx, %rdx
            #  r5 := r1 + r2
            mov  %rdx, %r11
            addq %r8, %r11
            #  r4 := r5
            mov  %r11, %r10
            #  i0 := r4
            mov  %r10, 16(%rbp)
            #  ret
            mov  %rbp, %rsp
            pop  %rbp
            ret

            .globl main
            .type main, @function
    main:
            push %rbp
            mov  %rsp, %rbp
            sub  $8, %rsp
            call my_module$main
            mov  -8(%rbp), %rax
            add  $8, %rsp
            mov  %rbp, %rsp
            pop  %rbp
            ret

