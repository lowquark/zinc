
## Atomicity and function arguments

Say arguments are always immutable. Arguments will be implicitly const qualified, so:

    function test(BigStruct input) -> BigStruct { // ...

is the same as:

    function test(const BigStruct input) -> BigStruct { // ...


BigStruct is really big, and it isn't declared atomic. Hence, we may pass it by reference like so:

    function test(const BigStruct input) -> BigStruct { // ...

is the same as:

    function test(const BigStruct & input) -> BigStruct { // ...


We're returning a BigStruct too. Again, it's big, and it isn't declared atomic. Eliding unecessary
copying, we can pass return space by reference and assign during the return statement. Hence,

    function test(const BigStruct & input) -> BigStruct { // ...

is the same as:

    function test(BigStruct & __return0, const BigStruct & input) { // ...


Therefore, 

    function test(BigStruct input) -> BigStruct { // ...

is the same as:

    function test(BigStruct & __return0, const BigStruct & input) { // ...

Q.E.D.

---

## Summary

| Type class:                      | Pass by:          | Return by:       |
| ----------------------------     | ---------------   | ---------------  |
| Atomic type                      | Value             | Value            |
| Atomic type, const               | Value             | Value            |
| Atomic type, reference           | Reference         | Reference        |
| Atomic type, const reference     | Const reference   | Const reference  |
| Non-atomic type                  | *Const reference* | *Copy elision*   |
| Non-atomic type, const           | *Const reference* | *Copy elision*   |
| Non-atomic type, reference       | Reference         | Reference        |
| Non-atomic type, const reference | Const reference   | Const reference  |

References are straightforward. Non-atomic types, however, are promoted to const references when
passed as arguments. This is made possible if non-atomic, non-reference arguments are said to be
immutable. It would be most consistent if all arguments were said to be immutable. Hence, marking a
non-reference argument as const is redundant.

Next steps:
 1) Determination of atomicity
 2) Allocation system
      - Atomic types -> Registers
      - Non-atomic types -> Stack

