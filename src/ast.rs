use crate::pool::{Ident, StringPool};
use std::{
    collections::HashMap,
    fmt::{format, Debug},
    hash::Hash,
    marker::PhantomData,
    rc::Rc,
    sync::RwLock,
};

#[derive(Clone, PartialEq)]
pub enum Expr<'p> {
    Num(f64),
    Call(Box<Self>, Vec<Self>),
    GetVar(Ident<'p>),
    Block(Vec<Stmt<'p>>, Box<Self>),
    IfElse(Box<Self>, Box<Self>, Box<Self>),
    Array(Vec<Self>),
}

#[derive(Clone, PartialEq)]
pub enum Stmt<'p> {
    Eval(Expr<'p>),
    DeclVar(Ident<'p>),
    SetVar(Ident<'p>, Expr<'p>),
}

impl<'p> Stmt<'p> {
    pub fn log(&self, pool: &StringPool) -> String {
        match self {
            &Stmt::DeclVar(i) => format!("let {};", pool.get(i)),
            Stmt::Eval(e) => e.log(pool),
            Stmt::SetVar(i, e) => format!("{} = {}", pool.get(*i), e.log(pool)),
        }
    }
}

impl<'p> Expr<'p> {
    pub fn log(&self, pool: &StringPool) -> String {
        match self {
            Expr::Num(n) => n.to_string(),
            Expr::Call(func, args) => {
                let args: Vec<_> = args.iter().map(|e| e.log(pool)).collect();
                let args: String = args.join(", ");
                format!("{}({})", func.log(pool), args)
            }
            &Expr::GetVar(i) => pool.get(i).to_string(),
            Expr::Block(es, val) => {
                let es: Vec<_> = es.iter().map(|e| e.log(pool)).collect();
                let es = es.join("; ");
                format!("{{ {}; {} }}", es, val.log(pool))
            }
            Expr::IfElse(cond, yes, no) => format!(
                "if {} {{ {} }} else {{ {} }}",
                cond.log(pool),
                yes.log(pool),
                no.log(pool)
            ),
            Expr::Array(args) => {
                let args: Vec<_> = args.iter().map(|e| e.log(pool)).collect();
                let args: String = args.join(", ");
                format!("array({})", args)
            }
        }
    }
}
