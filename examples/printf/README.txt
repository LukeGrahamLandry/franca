// https://github.com/rust-lang/rust/blob/master/compiler/rustc_builtin_macros/src/format.rs
// https://github.com/ziglang/zig/blob/master/lib/std/fmt.zig
// https://github.com/nim-lang/Nim/blob/devel/lib/pure/strformat.nim
// https://github.com/fmtlib/fmt/blob/master/include/fmt/format.h
// https://git.musl-libc.org/cgit/musl/tree/src/stdio/vfprintf.c
// https://github.com/openjdk/jdk/blob/master/src/java.base/share/classes/java/lang/invoke/StringConcatFactory.java

// TODO: @cache for functions like this and the bindings generator / parser where 
//       there's a comptime function whose output is syntax and you just put that in a file 
//       and check the hash of the input. both cases where im interested in cheating are string -> string. 
//       do i also need to check the hash of the function source code? 
//       i need a way to know if modules changed anyway. 
//       store last useage date and which file used it so you can clean up disk space. 
