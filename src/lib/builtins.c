// Name Mangling: <name> <param types underscore separated> __ <return type>

#define bin_op(name, ty, op)                                                   \
  ty name##_##ty##_##ty__##ty(ty a, ty b) { return a op b; }

#define bin_cmp(name, ty, op)                                                  \
  bool name##_##ty##_##ty__bool(ty a, ty b) { return a op b; }

typedef _Bool bool;
typedef double f64;
typedef long i64;

#define number(ty)                                                             \
  bin_op(add, ty, +);                                                          \
  bin_op(sub, ty, -);                                                          \
  bin_op(mul, ty, *);                                                          \
  bin_op(div_unchecked, ty, /);                                                \
  bin_cmp(gt, ty, >);                                                          \
  bin_cmp(lt, ty, <);                                                          \
  bin_cmp(eq, ty, ==);                                                         \
  bin_cmp(le, ty, <=);                                                         \
  bin_cmp(ge, ty, >=);                                                         \
  bin_cmp(me, ty, !=);

number(f64);
number(i64);

i64 cast_f64__i64(f64 n) { return n; }
f64 cast_i64__f64(i64 n) { return n; }

bin_op(or, bool, ||);
bin_op(and, bool, &&);
bin_cmp(eq, bool, ==);
bin_cmp(ne, bool, !=);
