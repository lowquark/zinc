
# Multiple assignment of objects

When assigning multiple rvalues to multiple lvalues at once, a problem arises when previous
assignments interfere with future assignments. Specifically, without saving the value of `a`, the
following multiple assignment will not swap `a` and `b`:

    a, b = b, a;

Instead, it will be equivalent to the following:

    a = b;
    b = a;

Wherein both `a` and `b` will have been assigned to the value of `b`.

## Solution

The straightforward solution to this problem is dead simple, and it works for all types. The
assignment is broken down into two phases:

 1) Copy rvalues into temporary storage
 2) Move saved rvalues into lvalues

### Example

Assume `a1` and `a2` are atomic structs with two fields each. Assume `b1` and `b2` are integers,
`c1` and `c2` are simple structs, and `d1` and `d2` are complex structs of type D. We seek to
perform the following, straighforward multiple assignment:

    a1, b1, c1, d1 = a2, b2, c2, d2

First, the rvalues are **copied** into temporary storage:

    a2_tmp.0 := a2_tmp.0
    a2_tmp.1 := a2_tmp.1
    b2_tmp := b2
    memcpy(c2_tmp, c2)
    D.copy(d2_tmp, d2);

Then, the saved values are **moved** into their respective lvalues:

    a1.0 := a2_tmp.0
    a1.1 := a2_tmp.1
    b1 := b2_tmp
    memcpy(c1, c2_tmp)
    D.move(d1, d2_tmp);

For all types but complex structs, a move operation is the same as a copy.

### Optimizations

There are certainly optimizations one could make: not every rvalue needs to be saved, and changing
the assignment order can reduce the number of saved values required.

A direct approach to eliminating unnecessary temporary storage would be to examine each rvalue in
order, and to compare it with the lvalues which have come before it for conflict. Only the rvalues
which are *possibly* set by the assignment of preceding lvalues need to be saved.

As far as ordering is concerned, there is probably a simple way to find an optimal ordering, but for
now, the general case is sufficient. Maybe you could sort by the depth in some dependency tree, but
weighted by the cost of copying?

