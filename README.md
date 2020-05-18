
# Zinc

Zinc is my experimental, C-family language.

## Features

Inspired by C, C++, and Lua, I've dreamt up a very simple, systems-level programming language with
the following features:

  - Namespaces
  - Default initialization
  - Multiple assignment & return values
  - Lvalue references (C++)
  - Built-in data structures
  - Write-access restrictions

One notable _anti-feature_ is that the language doesn't have true objects ---the kind with
constructors, methods, and inheritance. Instead, it loosely associates structs to particular groups
of functions through a novel write-access concept.

It's not much yet, and it's going to change, but I'm hoping to put these features together into a
cohesive programming language.

### Multiple assignment & return values

It's possible I've had too much fun in Lua lately, but having multiple return values can make
returning extra information very convenient:

    x, x_is_valid = complex_task(...);
    if(x_is_valid) {
      // ... use x
    }

Not to mention, swapping in a one-liner is pretty satisfying, too.

    a, b = b, a;

_Disclaimer: Because `=` invokes copying, using a dedicated swap operation would be a lot more
efficient for containers like `vector`._

Abstractly, having multiple return values makes APIs more symmetric in terms of input and
output. I think it would be nice to have in a systems language.

### Built-in data structures

I've spent a lot of time implementing data structures in C, so much that people have made fun of me
for it. Even still, I haven't found a satisfying way to implement them generically. (If anyone had,
maybe C wouldn't be so notoriously buggy.) Part of me has decided that complex data structures
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

where `append(...)` is a magic built-in function, kind of like `sizeof(...)`. By making a few
assumptions about the contents of contained objects (see below), data structures like these should
be possible without exposing destructors, copy constructors, and assignment operators into the
language itself. It's a lot of effort, but I think it _just might work_.

### Lvalue references

I've been mulling over an oddball sort of _anti-feature_ that might just be cool in practice.
Instead of proper C pointers, I want to experiment with C++-_style_ lvalue references that you're
not allowed to store in structs:

    struct MyStruct {
      const OtherStruct & other_struct; // This is an error, no references outside of functions
    }

    function main() {
      // These are ok
      const OtherStruct & other_struct = find_the_other(); // Returns a reference to an object
      do_something(other_struct) // Passed by reference (or copied, depending on signature)
    }

I'm not going to argue with you, this is pretty restrictive. But I also think there are some
reasonable, data-oriented workarounds for pointers. For example, you can always store the array
index of a neighboring struct, instead of storing its memory address directly.

By making these sacrifices, though, the compiler can generate copy constructors, move constructors,
and automatic serialization very easily. The implementation of built-in data structures is
simplified too. Who knows? It just might be useable.

### Write-access restrictions 

For a given struct type, only the code within certain modules has the ability to modify structs of
that type. Modules which contain those functions are said to have *access* to that type. The
simplest way to describe it is with a code demonstration:

    struct MyStruct {
      int a;
      access my_module;     // MyStruct has granted access to my_module
    }

    module my_module {
      function foo() {
        MyStruct my_struct;
        my_struct.a = 5;    // OK - my_module has access to MyStruct. 
      }
    }

    module other_module {
      function bar() {
        MyStruct my_struct;
        my_struct.a = 5;     // Not OK - other_module does not have access to MyStruct.
      }
    }

This is the most primitive form of encapsulation that I can think of. What's of particular interest
to me, is how one can use this feature to structure code without having to draw hard lines between
objects. Instead, the idea is that write-access restriction just kind of flows from the top down.

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
      access big;
    }

    module big {
      function do_complex_thing(Big b) {
        foo_subsystem:do_foo(b);
        bar_subsystem:do_bar(b);
      }
    }

    module big:foo_subsystem {
      function do_foo(Big b) {
        // I still have access to data in b, but I have specific privileges within B
      }
    }

    module big:bar_subsystem {
      function do_bar(Big b) {
        // I still have access to data in b, but I have specific privileges within B
      }
    }

In this example, `struct Big` is assumed to be some kind of God class, which is divided into `Foo`
and `Bar` ---logically distinct concepts with fragile internal state that shouldn't be altered by
anyone who isn't in the loop. As part of its complex task, `big` divides up responsibility between
its subsystems, and it does so without sacrificing scope ---a method call would be entirely
restricted to the contents of `Foo` or `Bar`, requiring creation of a separate object API.

With the `access` concept, I'm hoping the compiler can help protect data integrity in complex
processes without the programmer having to divide everything up into objects, fretting over whether
they're robust in all possible use cases. Indeed, I think it would help structure code that doesn't
have a clear object-oriented representation, or one which would require extensive use of callbacks
and handlers. It might also turn out to be one giant mess. Who knows.

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

