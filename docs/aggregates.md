# Aggregate Types

Franca provides a few primative types (integers, floats, and pointers) 
which can be combined to create more interesting types. 

## Structs 

A struct has multiple values stored together and accessable with field names. 
There are two styles for declaring a struct: one with parens that looks like a function call
and one with braces that looks like a block. They mean the same thing. Franca has a nominal type 
system; different structs are different types even if they have the same fields. 

You create an instance of a struct type with an initializer expression like `(name = value, etc)`. 
Structs fields can have (constant) default values that are used if an initializer 
does not provide that field. It is a compile error if a field that does not have a default 
value is not specified in the initializer expression. 

Like other native languages, struct variables default to being stored on the thread's callstack 
unless you explicitly allocate dynamic memory and use that instead. 

```
S :: @struct {
  a: A;
  b: B = bb;
}

S :: @struct(a: A, b: B = bb);

s: S = (a = aa);
s: S = (a = aa, b = cc);
```

There are no private fields, no constructors, and no destructors. 
There are no methods but you can use structs as a namespace by 
defining constant fields (with `::`). 

A struct with one field has the same representation as that field's type. 
A struct with zero fields requires no memory (same representation as `void`), 
so you can use a `HashMap(K, void)` as a `HashSet(K)`. 

If all the fields have default values, you can have an empty initializer like `s: Foo = ();`, 
since all the fields have constant known values, this is equivalent to a memcpy from a blessed 
template version of the struct (even if it's not implemented that way). 

## Tuples

A tuple is a special kind of struct made by just wrapping values in parens. 
They can be destructured into multiple variables so they're useful when 
a function wants to have multiple return values. You can access a tuple's fields 
individually with the names `_0`, `_1`, etc. 

```
f :: fn() Ty(i64, i64) = (1, 2);

one, two := f();
```

- TODO: because I am bad at my job, the syntax for type annotations 
on destructuring declarations looks stupid: `(a: A, b: B) := ab;`. 
- TODO: because I am bad at my job, destructuring requires that all 
the destructured variables be new declarations (not assignment) and not shadowing. 

## Enums

An enum is a (type-safe) set of named values (generally integers). 

```
E :: @enum(i64) (a, b, c);

foo: E = .a;
foo = .b;
bar := E.b;
```

## Unions

A union stores one value from a set of types in overlapping memory. 
Since the union doesn't know which type it currently contains, 
you have to track that information seperatly. This is almost never what you want. 
It is extremely easy to accidentally invoke behaviour considered disrespectful. 

## Tagged Unions

This is what rust calls an enum. 
It's just a bit of sugar around a struct that contains an enum and a union. 
For example, you could write this: 

```
T :: @struct(
  tag: @enum(A, B),
  payload: @union(a: A, b: B),
);
```

but that's a bit tedious to use and can be written more succinctly as: 

```
T :: @tagged(a: A, b: B);
```

You can also use this with `@match` to access the payload safely. 
You can't forget to check the tag because only the correct branch will execute. 

```
t: T = (a = value_of_type_a);

@match(t) {
  fn a(it) => use_type_a(it);
  fn b(it) => use_type_b(it);
}
```

You can unsafely access the payload with dot syntax (same as a struct field). 
Currently the compiler does not insert a tag check when you do that so it's the 
same as using a union but with extra memory overhead (to store the tag). 
Assigning the payload with field syntax does not change the tag so you should 
only assign to the currently active member (doing otherwise is considered disrespectful). 

When a varient has a payload of void you can initialize it with just `.Name` like an enum. 

One special tagged union type is `Option(T)`, which is only special because it has syntax sugar: `?T`. 

## Arrays

An array is a fixed (statically known) size group of elements of the same type stored inline. 
The type is spelled `Array(T, i)` where `i` is the number of elements (like `[T; i]` in rust). 
When you pass an Array to a function, it makes a copy of the array (the caller will not see 
changes made by the callee). 

```
a: Array(i64, 3) = @array(0, 1, 2);
one := a&[1];
```

Indexing past the end of an array is considered disrespectful 
(and will trigger an assertion if you have bounds checks enabled). 

## Dynamic Collections

Often you will want to hold many values but you won't know how many at compile time. 
You may also want to add and remove values from a collection as your program runs. 
These are implemented in the library (not as magic language builtins), 
so you can just look at the code (see `lib/collections/`) to see what functions 
you can call. But here is a brief summary of what we provide. 

Like zig, the franca collections use explicit memory allocators so there 
will often be a Foo and a RawFoo for each collection type. The 
normal version will have you pass in an allocator to initialize it 
and then store that allocator internally for use by other functions. 
The raw version forces you to pass an allocator to each call that needs one. 

- Slice: Some contiguous memory (of runtime known size) represented by a pair of a length 
and a pointer to the first element. What c# calls a Span. Since this type is so common, 
it has sugar: `[]T`. 
- List/RawList: Append items and when you run out of space it will reallocate to be twice as big. 
Items are stored contiguously so you can access them as a slice. C++ calls this a vector, 
Go calls this a slice.
- HashMap/RawHashMap: Associate keys and values. The key type must have a hash function 
that maps them down to a smaller index space to look up a slot in the table. 
It's the kind where if the slot you want is taken you check the next one, not a linked list of buckets. 
- RawDeque: Same as a List but stores a beginning index as well so it's cheap to append to either end. 
Sepending on usage pattern, the items might not be stored contiguously in memory. They might be split 
on the end of the array and wrap around to the other side. So you'd need to trigger some copying 
if you want it as a single slice. 
- DynamicBitSet: like a List(bool) but using one bit per value instead of one byte per value. 
You cannot get a pointer to an individual value (since bits are not addressable). 
- BucketArray: A List of Lists. When you run out of space to append items, it makes a new list 
without reallocating the old ones. So items have a stable memory address (you can safely hold 
a pointer accross a resize). 

Indexing past the end of a collection is considered disrespectful 
(and will trigger an assertion if you have bounds checks enabled). 

Calling drop on a collection generally won't recursively call drop on it's elements. 
