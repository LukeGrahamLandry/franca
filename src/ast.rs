use crate::pool::Ident;
use std::{
    collections::HashMap, fmt::Debug, hash::Hash, marker::PhantomData, rc::Rc, sync::RwLock,
};

#[derive(Clone, PartialEq)]
pub enum Expr<'p> {
    Num(f64),
    Call(Box<Self>),
    GetVar(Ident<'p>),
    SetVar(Ident<'p>, Box<Self>),
    Block(Vec<Stmt<'p>>),
}

#[derive(Clone, PartialEq)]
pub enum Stmt<'p> {
    Decl(Ident<'p>),
    Eval(Expr<'p>),
}

impl<'p> Stmt<'p> {}

impl<'p> Expr<'p> {
    // pub fn log(&self) -> String {
    //     match self {
    //         Stmt::Block(b) => todo!(),
    //         Stmt::Decl(e) => write!(f, "let "),
    //         Stmt::Eval(e) => todo!(),
    //     }
    // }
}
