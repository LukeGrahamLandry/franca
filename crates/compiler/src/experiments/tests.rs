#[macro_export]
macro_rules! jit_test {
    ($test_func:ident) => {
        macro_rules! simple {
            ($name:ident, $arg:expr, $ret:expr, $src:expr) => {
                #[test]
                fn $name() {
                    $crate::logging::init_logs(&[$crate::logging::LogTag::ShowErr]);
                    $test_func(stringify!($name), $src, |f| {
                        let ret: i64 = f($arg);
                        assert_eq!(ret, $ret);
                    })
                    .unwrap();
                }
            };
        }

        simple!(trivial, (), 42, "@c_call fn main() i64 = { 42 }");
        simple!(
            trivial_indirect,
            (),
            42,
            "@c_call fn get_42() i64 = { 42 } @c_call fn main() i64 = { get_42() }"
        );
        simple!(test_ifa, true, 123, "@c_call fn main(a: bool) i64 = { (a, fn()=123, fn=456)!if }");
        simple!(test_ifb, false, 456, "@c_call fn main(a: bool) i64 = { (a, fn()=123, fn=456)!if }");
        simple!(math, 5, 20, "@c_call fn main(a: i64) i64 = { add(a, 15) }");
        simple!(
            test_while,
            4,
            10,
            r#"
            @c_call fn main(n: i64) i64 = {
                var a = 0;
                (fn()=ne(n, 0), fn()={
                    a = add(a, n);
                    n = sub(n, 1);
                })!while;
                a
            }"#
        );
        simple!(
            var_addr,
            3,
            10,
            r#"
            @c_call fn main(n: i64) i64 = {
                var a = n;
                var b = 0;
                let a_ptr = a!addr;
                let b_ptr = b!addr;
                b_ptr[] = add(a_ptr[], 7);
                b
            }"#
        );
        simple!(
            nested,
            (),
            91,
            r#"
                @c_call fn main() i64 = {
                    add(add(add(add(add(add(add(add(add(1, 2), 3), 4), add(5, 6)), 7), 8), add(9, add(10, 11))), 12), 13)
                }"#
        );

        /* TODO
        simple!(
            recursion,
            5,
            8,
            r#"
                @c_call fn main(n: i64) i64 = {
                    (le(n, 1),
                        fn = 1,
                        fn = add(main(sub(n, 1)), main(sub(n, 2))),
                    )!if
                }
                "#
        );
        */

        #[test]
        fn use_ptr() {
            $crate::logging::init_logs(&[$crate::logging::LogTag::ShowErr]);
            $test_func(
                "use_ptr",
                r#"@c_call fn main(a: Ptr(i64), b: Ptr(i64)) i64 = {
                        b[] = add(a[], 1);
                        add(b[], 4)
                    }"#,
                |f| {
                    let mut a = 5;
                    let mut b = 0;
                    let ret: i64 = f((addr_of!(a), addr_of!(b)));
                    assert_eq!(ret, 10);
                    assert_eq!(a, 5);
                    assert_eq!(b, 6);
                },
            )
            .unwrap();
        }

        #[test]
        fn ffi_ptr() {
            $crate::logging::init_logs(&[$crate::logging::LogTag::ShowErr]);
            $test_func(
                "ffi_ptr",
                r#"@c_call fn main(a: Ptr(SuperSimple)) i64 = {
                        let c: i64 = sub(a.b[], a.a[]);
                        a.a[] = 1;
                        a.b[] = 2;
                        c
                    }"#,
                |f| {
                    let mut a = SuperSimple { a: 57, b: 77 };
                    let ret: i64 = f(addr_of!(a));
                    assert_eq!(ret, 20);
                    assert_eq!(a.a, 1);
                    assert_eq!(a.b, 2);
                },
            )
            .unwrap();
        }

        // TODO: bootstrap raw_slice
        // simple!(basic, 3145, 3145, include_str!("../../tests/basic.txt"));
    };
}
macro_rules! jit_test_aarch_only {
    ($test_func:ident) => {
        macro_rules! simple {
            ($name:ident, $arg:expr, $ret:expr, $src:expr) => {
                #[test]
                fn $name() {
                    $crate::logging::init_logs(&[$crate::logging::LogTag::ShowErr]);
                    $test_func(stringify!($name), $src, |f| {
                        let ret: i64 = f($arg);
                        assert_eq!(ret, $ret);
                    })
                    .unwrap();
                }
            };
        }

        simple!(
            fields,
            3,
            10,
            r#"
            @c_call fn main(n: i64) i64 = {
                const A = (a: i64, b: i64)!struct;
                var a: A = (a: n, b: 0);
                a&.b[] = add(a&.a[], 7);
                a&.b[]
            }"#
        );

        /*
        simple!(
            varient,
            3,
            10,
            r#"
                @c_call fn main(n: i64) i64 = {
                    const A = (a: i64, b: i64)!enum;
                    var a: A = (a: n);
                    add(a&.a[], 7)
                }"#
        );

        simple!(
            varient_noop,
            3,
            3,
            r#"
                @c_call fn main(n: i64) i64 = {
                    const A = (a: i64, b: i64)!enum;
                    var a: A = (b: n);
                    a&.b[]
                }"#
        );
        */
        // simple!(
        //     use_any_reg,
        //     5,
        //     2,
        //     r#"
        //     @any_reg
        //     fn sub2(a: i64, b: i64) i64 = (fn(data: OpPtr, op: RetOp, r: Slice(u5)) Unit = {
        //         op(data, sub_sr(Bits.X64[], get(r, 2), get(r, 0), get(r, 1), Shift.LSL[], 0b000000));
        //     });

        //     @c_call fn main(a: i64) i64 = {
        //         sub2(a, 3)
        //     }"#
        // );

        simple!(
            assert_eq_ffi,
            (),
            2,
            r#"
            @c_call fn main() i64 = {
                assert_eq(1, 1);
                2
            }"#
        );

        // Requires being able to write constant heap values.
        simple!(
            use_str,
            (),
            5,
            r#"
            @c_call fn main() i64 = {
                let s: Str = "hello";
                len(s)
            }"#
        );

        simple!(backpassing, 5, 5, include_str!("../../../../tests/backpassing.fr"));
        // TODO: this relies on structs being in consecutive stack slots so had to disable reusing them.
        simple!(structs, 5, 5, include_str!("../../../../tests/structs.fr"));
        simple!(overloading, 5, 5, include_str!("../../../../tests/overloading.fr"));
        simple!(closures, 5, 5, include_str!("../../../../tests/closures.fr"));
        simple!(macros, 5, 5, include_str!("../../../../tests/macros.fr"));
        simple!(generics, 5, 5, include_str!("../../../../tests/generics.fr"));
        simple!(basic, 5, 5, include_str!("../../../../tests/basic.fr"));
        // simple!(collections, 5, 5, include_str!("../../tests/collections.fr"));

        simple!(
            backtrace,
            (),
            123,
            r#"
            @c_call fn thing2() i64 = {
                // TODO: return a data structure and actually test stuff with it. 
                collect_backtrace();
                123
            }
            @c_call fn thing1() i64 = {
                let _ = 1.add(2);
                thing2()
            }
            @c_call fn main() i64 = thing1();
            "#
        );
    };
}

#[macro_export]
macro_rules! jit_test_llvm_only {
    ($test_func:ident) => {
        macro_rules! simple {
            ($name:ident, $arg:expr, $ret:expr, $src:expr) => {
                #[test]
                fn $name() {
                    $crate::logging::init_logs(&[$crate::logging::LogTag::ShowErr]);
                    $test_func(stringify!($name), $src, |f| {
                        let ret: i64 = f($arg);
                        assert_eq!(ret, $ret);
                    })
                    .unwrap();
                }
            };
        }

        simple!(
            add_floats,
            3,
            3,
            r#"
            @c_call fn main(n: i64) i64 = {
                let a: f64 = add(1.0, 2.5);
                n
            }"#
        );

        // simple!(mandelbrot, (), 40, include_str!("../../../examples/mandelbrot.fr"));
    };
}

pub use jit_test;
pub(crate) use jit_test_aarch_only;
pub use jit_test_llvm_only;
