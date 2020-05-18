
# Zinc

Zinc is my experimental, C-family language.

## Features

As an embedded systems developer, I have a strong affinity for C, but am keenly aware of its
limitations. Here's a list of features I'm working toward:

  - Namespaces
  - Default initialization
  - Usable strings
  - Multiple assignment & return values
  - C++ lvalue references
  - Built-in data structures
  - Write-access restrictions

The first 3 are pretty obvious to anyone who's used C for a prolonged period of time. The rest I
describe in more detail below.

### Multiple assignment & return values

Multiple return values makes returning status codes, or additional, optional information very
convenient:

    x, x_is_valid = complex_task(...);
    if(x_is_valid) {
      // ... use x
    }

Not to mention, swapping in a one-liner is *supremely* satisfying:

    a, b = b, a;

Of course, because `=` invokes copying, using a dedicated `swap(...)` function would be far more
efficient for container objects like `vector` or `map`. It's possible I've had too much fun in Lua
and Python lately. In any case, this is a prominent feature I'm working toward.

### Built-in data structures

I've spent a lot of time implementing data structures in C. People have made fun of me for it.
Sadly, I haven't found a satisfying way to implement them generically. Part of me has decided they
should be provided directly by the language. For example:

    function main() {
      // Declares a vector of int64s
      vector<int64> my_list;
      append(my_list, 5);
      append(my_list, 5);
      append(my_list, 5);
      // Read 'em and weep
      for(int64 idx, int64 val in my_list) {
        print(idx, val);
      }
    }

where `append(...)` is a magic built-in function, kind of like `sizeof(...)`. Fundamentally, data
structures like these should be possible without exposing destructors, copy constructors, and
assignment operators into the language itself. I think it _just might work_.

### C++ lvalue references

I've been mulling over an oddball sort of _anti-feature_ that might just be cool in practice.
Instead of proper C pointers, I want to experiment with C++ lvalue references that you're not
allowed to store in structs:

    struct MyStruct {
      const OtherStruct & other_struct; // This is an error, no references outside of functions
    }

    function main() {
      // These are ok
      const OtherStruct & other_struct = find_the_other(); // Returns a reference to an object
      do_something(other_struct) // Passed by reference (or copied, depending on signature)
    }

This is pretty restrictive, but I think there are reasonable workarounds for pointers. For example,
you can always just store the array index of a neighboring struct, instead of storing its address
directly.

In return, the compiler can generate copy constructors, move constructors, and automatic
serialization very easily. The implementation of built-in data structures is simplified too. Who
knows? It just might be useful.

### Write-access restrictions 

This is another wierd one. The simplest way to describe this is with a code demonstration:

    struct MyStruct {
      int a;
      access module_a;       // MyStruct grants access to functions within module_a
    }

    module module_a {
      function foo() {
        MyStruct my_struct;
        my_struct.a = 5;     // module_a modifies a MyStruct - OK
      }
    }

    module module_b {
      function bar() {
        MyStruct my_struct;
        my_struct.a = 5;     // Not OK! module_b does not have access to MyStruct.
      }
    }

Basically, only certain modules are allowed to modify the contents of certain struct types ---those
which have *access* to that type. This is the most primitive form of encapsulation that I can think
of. What's interesting to me, is how one can use this feature to structure code without drawing hard
lines for which methods are part of which objects.

    struct Foo {
      // ...
      access big:foo_subsystem;
    }

    struct Bar {
      // ...
      access big:bar_subsystem;
    }

    struct Big {
      Foo foo;
      Bar bar;
    }

    module big {
      function do_complex_thing(Big b) {
        foo_subsystem:do_foo(b);
        bar_subsystem:do_bar(b);
      }
    }

    module big:foo_subsystem {
      function do_foo(Big b) {
        // I still have access to data in b, but I have specific priviledges within B
      }
    }

    module big:bar_subsystem {
      function do_bar(Big b) {
        // I still have access to data in b, but I have specific priviledges within B
      }
    }

In this example, `struct Big` is assumed to be some kind of God class, which is divided into `Foo`
and `Bar`: logically distinct concepts with fragile state that shouldn't be altered by anyone who
isn't in the loop. With the `access` concept, the compiler can help protect data integrity without
the programmer having to create independent, use-anywhere objects.

I have a feeling this should scale pretty well. I think it's worth giving a shot.

# znc (Zinc Compiler)

I have a few thousand lines of Lua that spit out x86-64 assembly. One day I'd like to have a C
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

