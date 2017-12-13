# cyclic

one-time allocation fixed memory nogc array implementation using a cyclic stack/malloc'd array with high efficiency putBack & putFront.

This is basically just a normal array without allocations and instead of having a continous stream of memory it can be cut off once.

Optimal for a dequeue with fairly reliable (but not guaranteed nor safe) multi-threaded access.

```d
// stack array, taking up to 4kb by default
CyclicArray!int array;
assert(array.length == 0);
array ~= 5;
assert(!array.empty);
assert(array.front == 5);
assert(array[0] == 5);
array ~= [4, 3];

assert(array == [5, 4, 3]);

// same efficiency as insertBack/put/concat
array.insertFront(5);

// heap array using std.container.Array with 16 elements
auto heapArray = CyclicArray!(int, 0)(16);

// custom memory using Array
auto myArray = Array!int(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
auto customArray = CyclicArray!(int, 0)(myArray[0 .. 6]);
```