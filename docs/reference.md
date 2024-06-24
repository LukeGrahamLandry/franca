## Declarations

```
constant : T : value;
variable : T = value;
infer_constant :: value;
infer_variable := value;
```

## Assignment

## Implicit Constant Coercion

## Function Expressions

## Function Calls

```
f(a, b); // normal call
a.f(b);  // dot call (desugars to the above. NOT a vtable lookup!)
```

## Aggregate Expressions

```
// tuples
(123, "123", 123.0);         Ty(i64, Str, f64);

// structs
(name = 123, c = "hello");   @struct(name: 123, hello: String);

// tagged unions
(varient = 123);             @tagged(varient: i64, another: bool, Empty: void);
.Empty;

// enum
.Name;                       @enum(i64) (Name, Other, Else);
```

## Function Overloading

## Operator Overloading

Arithmetic operators have thier conventional precidence and desugar to function calls.

Binary:

```
add, sub, mul, div, eq, ne, lt, gt, le, ge,
 +    -    *    /   ==  !=  <   >   <=  >=
```

Unary:

```
not, neg
 !    -
```
