#![allow(unused)]
use crate::{
    ast::{Expr, FatExpr, Program, WalkAst},
    bc::SizeCache,
    compiler::Res,
};

// - Pass result pointer to return structs.
// - Pass const pointer to arg instead of copying it somehow.
// - Pass result pointer into blocks.
// - Remove load/store of unit.
// TODO: this would be easier if i did it on the ir but cooler if you could see the transformed version of your program.
/// Help avoid memcpys, and many abis require it anyway,
enum CallTransform {
    // ReturnByRef { value: TypeId, ptr: TypeId },
    // ArgByRef { value: TypeId, ptr: TypeId, index: usize },
}

struct SRet<'a, 'p> {
    program: &'a mut Program<'p>,
    size_cache: SizeCache,
}

// impl<'a, 'p> SRet<'a, 'p> {
//     fn transform(&mut self, expr: &mut FatExpr<'p>, result_ptr: FatExpr<'p>) -> Res<'p, ()> {
//         if self.size_cache.slot_count(self.program, expr.ty) <= 1 {
//             return Ok(());
//         }
//         todo!()
//     }
// }

impl<'a, 'p> WalkAst<'p> for SRet<'a, 'p> {
    fn pre_walk_expr(&mut self, expr: &mut FatExpr<'p>) {
        if let Expr::Call(f, a) = &mut expr.expr {}
    }

    fn post_walk_expr(&mut self, _: &mut FatExpr<'p>) {}
}
