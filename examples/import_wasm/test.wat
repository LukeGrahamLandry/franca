(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func))
  (type (;2;) (func (param i32 i32) (result i32)))
  (import "host" "write" (func (;0;) (type 0)))
  (import "host" "mem" (memory 1))
  (func (;1;) (type 1)
    i32.const 0
    i32.const 12
    call 0)
  (func (;2;) (type 2)
    (local.get 0)
    (local.get 1)
    i32.add
    )
  (export "main" (func 1))
  (data (i32.const 0) "Hello World!"))

(; wat2wasm examples/import_wasm/test.wat -o a.wasm ;)
(; franca examples/import_wasm/convert.fr a.wasm ;)
