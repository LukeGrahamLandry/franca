#![allow(unused)]
use crate::ast::{Expr, FatExpr, Program, WalkAst};

// TODO: this would be easier if i did it on the ir but cooler if you could see the transformed version of your program.
/// Help avoid memcpys, and many abis require it anyway,
enum CallTransform {
    // ReturnByRef { value: TypeId, ptr: TypeId },
    // ArgByRef { value: TypeId, ptr: TypeId, index: usize },
}

struct SRet<'a, 'p> {
    program: &'a mut Program<'p>,
}

impl<'a, 'p> WalkAst<'p> for SRet<'a, 'p> {
    fn walk_expr(&mut self, expr: &mut FatExpr<'p>) {
        if let Expr::Call(f, a) = &mut expr.expr {}
    }

    fn post_walk_expr(&mut self, _: &mut FatExpr<'p>) {}
}
