
# Zinc

Inspired by C, C++, and Lua, I've come up with a very simple, systems-level programming language
with the following features:

  - Namespaces
  - Default initialization
  - Multiple assignment & return values
  - Lvalue references (C++)
  - Built-in data structures
  - Write-access restriction (_access_)

One notable _anti-feature_ is that the language doesn't have true objects ---the kind with
constructors, methods, and inheritance. Instead, it loosely associates structs to particular groups
of functions through a novel _access_ concept that I describe somewhat below.

It's not much yet, and it's going to change, but I'm hoping to put these features together into a
cohesive programming language. Namespaces and default initialization are exactly what you'd expect,
so I won't waste your time describing them here.

### Multiple assignment & return values

Multiple return values makes returning extra information from a function very convenient:

    int x, Status status = complex_task(...);
    if(status == Status:OK) {
      // ... use x
    }

I'd have to use an optional wrapper, or a dedicated struct to pull this off in C or C++. Not to
mention, swapping in a one-liner is pretty satisfying, too:

    a, b = b, a;

_Disclaimer: Because `=` invokes copying, using a dedicated swap operation would be a lot more
efficient for containers like `vector`._

I've implemented a few revisions of this feature, and I haven't found any reasons it shouldn't be
supported (other than the fact that it implies the use of a heavily nonstandard ABI). Personally, I
think it would be nice to have in a low-level language.

### Built-in data structures

I've spent a lot of time implementing data structures in C, to the point that people make fun of me.
Even still, I haven't found a satisfying way to implement them generically. (If anyone had, maybe C
wouldn't be so notoriously buggy.) Part of me has decided that complex data structures should be
provided directly by the language. For example:

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

### Write-access restriction (_access_)

This is an original concept that I am the most interested in building, and I can't say I've seen it
in the wild before. It's a very minimal form of procedural-style encapsulation, with a few
architectural side-effects that I think are worth exploring.

It boils down to this: each struct declares which modules (namespaces) are allowed to modify it. In
other words, the struct specifies which modules have *access* to it. Anyone who doesn't have access
sees the struct's fields as read-only. The simplest way to describe it is with a code demonstration:

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
        my_struct.a = 5;    // Not OK - other_module does not have access to MyStruct.
      }
    }

It's kind of like what a `friend namespace` would be, if ever there was such a thing.

What's of particular interest to me, is how one can use this feature to structure code without
having to draw hard lines between objects. Instead, it seems that access restrictions just kind of
flow from the top down:

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
        // We can only read from b here, but our subordinates can modify it!
        foo_subsystem:do_foo(b);
        bar_subsystem:do_bar(b);
      }
    }

    module big:foo_subsystem {
      function do_foo(Big b) {
        // We can modify b.foo here (but only read from b.bar)
      }
    }

    module big:bar_subsystem {
      function do_bar(Big b) {
        // We can modify b.bar here (but only read from b.foo)
      }
    }

In this example, `struct Big` is assumed to be some kind of God class, which is divided into `Foo`
and `Bar`: logically distinct concepts with fragile internal state that shouldn't be altered by
anyone who isn't in the loop. As part of its complex task, `big` divides up responsibility between
its subsystems, and it does so without sacrificing scope ---a standard OOP method call would be
entirely restricted to the contents of `Foo` or `Bar`, requiring additional API complexity.

With the `access` concept, I'm hoping the compiler can help protect data integrity in complex
processes without the programmer having to divide everything up into objects, fretting over whether
they're robust in all possible use cases. Indeed, I think it would help structure code that doesn't
have a clear object-oriented representation, or one which would require extensive use of callbacks
and handlers.

Moreover, I can see subsystems like `Foo` and `Bar` fluidly growing into their own distinct objects
as the codebase grows. I can also see it turning out to be one giant mess of procedural code. Who
knows?

# znc (Zinc Compiler)

I have a few thousand lines of Lua that spit out x86-64 assembly. Don't worry, one day I'll take a
shot at writing it in C.

If you have GCC and Lua installed on a 64-bit Linux machine, clone the repository, navigate to
`test/`, and run `./test-all` to run the unit tests.

Otherwise, here's some sexy compiler output:

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
        int64 astro, v = bla(bla(r, r, r), u, v);
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
                LVALUE
                  REFERENCE a
                LVALUE
                  REFERENCE b
              LVALUE
                REFERENCE c
            LVALUE
              REFERENCE a
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
              CALL bla
                CALL bla
                  LVALUE
                    REFERENCE r
                  LVALUE
                    REFERENCE r
                  LVALUE
                    REFERENCE r
                LVALUE
                  REFERENCE u
                LVALUE
                  REFERENCE v
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
              LVALUE
                REFERENCE b
              LVALUE
                REFERENCE a
          ASSIGNMENT
            REFERENCE astro
            :=
              ADD 
                ADD 
                  LVALUE
                    REFERENCE astro
                  LVALUE
                    REFERENCE b
                LVALUE
                  REFERENCE v
          RETURN
            LVALUE
              REFERENCE astro

### Intermediate representation:

    sub z$bla (3) -> (2)
      s0 := a0 + a1
      s1 := s0 + a2
      b0 := s1
      b1 := a0
      ret
    sub z$main (0) -> (1)
      s0 := 3
      s1 := 2
      s2 := 4
      s4 := z$bla (s0, s0, s0)
      s3, s2 := z$bla (s4, s1, s2)
      s7 := 1
      s8 := 0
      s5 := s7
      s6 := s8
      s9 := s6
      s10 := s5
      s5 := s9
      s6 := s10
      s11 := s3 + s6
      s12 := s11 + s2
      s3 := s12
      b0 := s3
      ret

### Liveness analysis and linear scan register allocation:

    Allocating hardware registers for IR subroutine: `z$bla`
    s0 is live on [1, 2)
    s1 is live on [2, 3)
    map s0 -> r0
    map s1 -> r1

    Resultant IR:
    sub z$bla (3) -> (2)
      r0 := a0 + a1
      r1 := r0 + a2
      b0 := r1
      b1 := a0
      ret

    Allocating hardware registers for IR subroutine: `z$main`
    s0 is live on [1, 4)
    s1 is live on [2, 5)
    s2 is live on [3, 15)
    s4 is live on [4, 5)
    s3 is live on [5, 17)
    s7 is live on [6, 8)
    s8 is live on [7, 9)
    s5 is live on [8, 11)
    s6 is live on [9, 14)
    s9 is live on [10, 12)
    s10 is live on [11, 13)
    s11 is live on [14, 15)
    s12 is live on [15, 16)
    map s0 -> r0
    map s1 -> r1
    map s2 -> r2
    map s4 -> r3
    map s3 -> r4
    map s7 -> r5
    map s8 -> r0
    map s5 -> r3
    map s6 -> r1
    map s9 -> r5
    map s10 -> r0
    map s11 -> r3
    map s12 -> r5

    Resultant IR:
    sub z$main (0) -> (1)
      r0 := 3
      r1 := 2
      r2 := 4
      r3 := z$bla (r0, r0, r0)
      r4, r2 := z$bla (r3, r1, r2)
      r5 := 1
      r0 := 0
      r3 := r5
      r1 := r0
      r5 := r1
      r0 := r3
      r3 := r5
      r1 := r0
      r3 := r4 + r1
      r5 := r3 + r2
      r4 := r5
      b0 := r4
      ret

### Resulting assembly (AT&T Syntax):

            .text

            .globl z$bla
            .type z$bla, @function
    z$bla:
            push %rbp
            movq %rsp, %rbp
            subq $16, %rsp
            #  r0 := a0 + a1
            movq 16(%rbp), %rbx
            addq 24(%rbp), %rbx
            #  r1 := r0 + a2
            movq %rbx, %rdx
            addq 32(%rbp), %rdx
            #  b0 := r1
            movq %rdx, 40(%rbp)
            #  b1 := a0
            movq 16(%rbp), %rax
            movq %rax, 48(%rbp)
            #  ret
            movq %rbp, %rsp
            pop  %rbp
            ret

            .globl z$main
            .type z$main, @function
    z$main:
            push %rbp
            movq %rsp, %rbp
            subq $64, %rsp
            #  r0 := 3
            movq $3, %rbx
            #  r1 := 2
            movq $2, %rdx
            #  r2 := 4
            movq $4, %r8
            #  r3 := z$bla (r0, r0, r0)
            push %rbx
            push %rdx
            push %r8
            push %r10
            push %r11
            subq $40, %rsp
            movq %rbx, 0(%rsp)
            movq %rbx, 8(%rsp)
            movq %rbx, 16(%rsp)
            call z$bla
            movq 24(%rsp), %r9
            addq $40, %rsp
            pop %r11
            pop %r10
            pop %r8
            pop %rdx
            pop %rbx
            #  r4, r2 := z$bla (r3, r1, r2)
            push %rbx
            push %rdx
            push %r9
            push %r11
            subq $40, %rsp
            movq %r9, 0(%rsp)
            movq %rdx, 8(%rsp)
            movq %r8, 16(%rsp)
            call z$bla
            movq 24(%rsp), %r10
            movq 32(%rsp), %r8
            addq $40, %rsp
            pop %r11
            pop %r9
            pop %rdx
            pop %rbx
            #  r5 := 1
            movq $1, %r11
            #  r0 := 0
            movq $0, %rbx
            #  r3 := r5
            movq %r11, %r9
            #  r1 := r0
            movq %rbx, %rdx
            #  r5 := r1
            movq %rdx, %r11
            #  r0 := r3
            movq %r9, %rbx
            #  r3 := r5
            movq %r11, %r9
            #  r1 := r0
            movq %rbx, %rdx
            #  r3 := r4 + r1
            movq %r10, %r9
            addq %rdx, %r9
            #  r5 := r3 + r2
            movq %r9, %r11
            addq %r8, %r11
            #  r4 := r5
            movq %r11, %r10
            #  b0 := r4
            movq %r10, 16(%rbp)
            #  ret
            movq %rbp, %rsp
            pop  %rbp
            ret

            .globl main
            .type main, @function
    main:
            push %rbp
            movq %rsp, %rbp
            subq $8, %rsp
            call z$main
            movq -8(%rbp), %rax
            addq $8, %rsp
            movq %rbp, %rsp
            pop  %rbp
            ret

