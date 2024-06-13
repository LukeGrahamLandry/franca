//! Convert a string of source code into a stream of tokens.
// TODO: ignore shabang line and make sure compiler supports running scripts that way. thats a good side affect of the "all info needed in program" goal.
// TODO: this could be a good thing to start with for trying self hosting.

use crate::ast::Flag;
use crate::ast::TypeId;
use crate::ast::VarType;
use crate::lex::TokenType::*;
use crate::pool::{Ident, StringPool};
use codemap::{File, Span};
use interp_derive::InterpSend;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::ops::Deref;
use std::str::Chars;
use std::sync::Arc;

// TODO: why tf is Range not copy
#[derive(Debug, Clone)]
pub struct Token<'p> {
    pub kind: TokenType<'p>,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType<'p> {
    Symbol(Ident<'p>),
    Number(i64),
    Float(f64),
    Quoted { s: Ident<'p>, escapes: bool },
    DotLeftSquiggle,
    LeftSquiggle,
    RightSquiggle,
    LeftParen,
    RightParen,
    LeftSquare,
    RightSquare,
    Bang,
    Dot,
    Fn,
    At,
    Comma,
    Colon,
    Slash,
    Equals,
    Eof,
    Qualifier(VarType),
    Semicolon,
    Star,
    UpArrow,
    Amp,
    Question,
    Dollar,
    DoubleSquare,
    DoubleColon,
    Pipe,
    // Bit count means leading zeros are observable.
    BinaryNum { bit_count: u8, value: u64 },
    LeftArrow,
    RightArrow,
    Quote,
    LeftAngle,
    RightAngle,
    Hash,
    FatRightArrow,
    EqOp(Flag),
    Error(LexErr),
}

#[derive(Debug, Clone, Copy, PartialEq, InterpSend)]
pub enum LexErr {
    UnterminatedStr,
    Unexpected(char),
    NumParseErr,
    /// C has it mean octal which I don't really like but I don't want to accept the same syntax and mean something different.
    DenyLeadingZero,
    ThatsNotANormalQuoteDidYouCopyPasteFromAPdf,
    TooManyBits,
}

pub struct Lexer<'a, 'p> {
    pool: &'p StringPool<'p>,
    root: Span,
    pub src: Arc<File>,
    start: usize,
    current: usize,
    chars: Peekable<Chars<'a>>,
    peeked: VecDeque<Token<'p>>,
    hack: Option<Token<'p>>,
}

impl<'a, 'p> Lexer<'a, 'p> {
    pub fn new(src: Arc<File>, pool: &'p StringPool<'p>, root: Span) -> Self {
        // Safety: its in an arc which is dropped at the same time as the iterator.
        let code = src.source_slice(root);
        let hack = unsafe { &*(code as *const str) };
        Self {
            src,
            start: 0,
            current: 0,
            peeked: VecDeque::with_capacity(10),
            chars: hack.chars().peekable(),
            root,
            pool,
            hack: None,
        }
    }

    pub(crate) fn skip_to_closing_squigle(&mut self) -> Span {
        let t = self.next();
        debug_assert_eq!(t.kind, LeftSquiggle);
        let mut depth = 1;
        let start = self.start;
        debug_assert!(self.peeked.is_empty());

        while depth > 0 {
            self.eat_whitespace();
            self.start = self.current;
            // TODO: skip #! ...... \n but it only matters if you had unnested stuff there which you shouldn't -- Apr 28
            match self.peek_c() {
                '\0' => break,
                '"' => {
                    self.skip_quoted();
                }
                '{' => {
                    depth += 1;
                    self.pop();
                }
                '}' => {
                    depth -= 1;
                    self.pop();
                }
                _ => {
                    self.pop();
                }
            }
        }
        let end = self.current;
        self.start = self.current;
        self.root.subspan(start as u64, end as u64)
    }

    // pop the first buffered token or generate a new one
    #[allow(clippy::should_implement_trait)] // nobody cares bro
    pub fn next(&mut self) -> Token<'p> {
        if let Some(prev) = self.peeked.pop_front() {
            return prev;
        }

        todo!("untested"); // i guess i always happen to peek first in the parser.
    }

    fn do_next(&mut self) -> Token<'p> {
        self.eat_whitespace();
        self.start = self.current;
        match self.peek_c() {
            '\0' => self.one(TokenType::Eof),
            '#' => {
                self.pop();
                match self.peek_c() {
                    '!' => {
                        while self.pop() != '\n' {
                            // spin
                        }
                        self.do_next()
                    }
                    _ => self.token(Hash, self.start, self.current),
                }
            }
            '"' => self.lex_quoted(),
            '0' => {
                self.pop();
                match self.peek_c() {
                    'b' => {
                        self.pop();
                        self.lex_bin()
                    }
                    'x' => {
                        self.pop();
                        self.lex_hex()
                    }
                    '.' => self.lex_num(),
                    '0'..='9' => self.err(LexErr::DenyLeadingZero),
                    _ => self.token(Number(0), self.start, self.current),
                }
            }
            '1'..='9' => self.lex_num(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_ident(),
            '{' => self.one(LeftSquiggle),
            '}' => self.one(RightSquiggle),
            '(' => self.one(LeftParen),
            ')' => self.one(RightParen),
            ',' => self.one(Comma),
            ':' => self.pair(':', Colon, DoubleColon),
            '@' => self.one(At),
            '.' => self.pair('{', Dot, DotLeftSquiggle),
            '!' => self.one(Bang),
            '[' => self.pair(']', LeftSquare, DoubleSquare),
            ']' => self.one(RightSquare),
            ';' => self.one(Semicolon),
            '=' => self.pair('>', Equals, FatRightArrow),
            '*' => self.pair('=', Star, EqOp(Flag::Operator_Star_Equal)),
            '^' => self.one(UpArrow),
            '&' => self.one(Amp),
            '?' => self.one(Question),
            '$' => self.one(Dollar),
            '|' => self.one(Pipe),
            '/' => self.pair('=', Slash, EqOp(Flag::Operator_Slash_Equal)),
            '+' => self.pair('=', Error(LexErr::Unexpected('+')), EqOp(Flag::Operator_Plus_Equal)),
            '-' => self.pair('=', Error(LexErr::Unexpected('-')), EqOp(Flag::Operator_Minus_Equal)),
            '<' => self.pair('-', LeftAngle, LeftArrow),
            '>' => self.pair('-', RightAngle, RightArrow),
            '\'' => self.one(Quote),
            '“' | '”' => self.err(LexErr::ThatsNotANormalQuoteDidYouCopyPasteFromAPdf),
            c => {
                if !c.is_ascii() && !c.is_whitespace() {
                    self.lex_ident()
                } else {
                    self.err(LexErr::Unexpected(c))
                }
            }
        }
    }

    fn pair(&mut self, second: char, single: TokenType<'static>, double: TokenType<'static>) -> Token<'p> {
        self.pop();
        let t = if self.peek_c() == second {
            self.pop();
            double
        } else {
            single
        };
        self.token(t, self.start, self.current)
    }

    // generate enough to look ahead i tokens.
    pub fn nth(&mut self, i: usize) -> Token<'p> {
        while self.peeked.len() <= i {
            let t = self.do_next();
            self.peeked.push_back(t);
            if let Some(t) = self.hack.take() {
                self.peeked.push_back(t);
            }
        }
        self.peeked[i].clone()
    }

    fn one(&mut self, t: TokenType<'static>) -> Token<'p> {
        self.pop();
        self.token(t, self.start, self.current)
    }

    fn token(&self, kind: TokenType<'p>, start: usize, end: usize) -> Token<'p> {
        Token {
            kind,
            span: self.root.subspan(start as u64, end as u64),
        }
    }

    // TODO: support escape characters
    fn lex_quoted(&mut self) -> Token<'p> {
        if let Some(((start, end), escapes)) = self.skip_quoted() {
            let text = &self.src.source_slice(self.root)[start..end];
            // TODO: probably dont want to always hash string constants like this.
            self.token(
                Quoted {
                    s: self.pool.intern(text),
                    escapes,
                },
                self.start,
                self.current,
            )
        } else {
            self.err(LexErr::UnterminatedStr)
        }
    }

    fn skip_quoted(&mut self) -> Option<((usize, usize), bool)> {
        self.pop();
        let mut contains_escapes = false;
        loop {
            match self.pop() {
                '"' => {
                    // Payload doesn't include quotes.
                    return if self.start + 2 == self.current && self.peek_c() == '"' {
                        let mut count = 0;
                        while count < 3 {
                            match self.pop() {
                                '"' => count += 1,
                                '\0' => return None,
                                _ => count = 0,
                            }
                        }
                        Some(((self.start + 3, self.current - 3), false))
                    } else {
                        Some(((self.start + 1, self.current - 1), contains_escapes))
                    };
                }
                '\\' => {
                    self.pop(); // an extra so you can do \".
                    contains_escapes = true;
                }
                // I could let you have multi-line, but I don't like it because you end up with garbage indentation.
                '\n' | '\0' => return None,
                _ => {}
            }
        }
    }

    fn lex_num(&mut self) -> Token<'p> {
        let mut is_float = false;
        loop {
            match self.peek_c() {
                '.' => {
                    if is_float {
                        return self.end_num(true);
                    }
                    // TODO: this would be easier if i could just peek a char.
                    let int = self.end_num(false);
                    self.pop();
                    if self.peek_c().is_numeric() {
                        is_float = true;
                    } else {
                        self.start = self.current;
                        debug_assert!(self.hack.is_none());
                        // cant just use self.peeked because self.nth will push whatever we return.
                        self.hack = Some(self.token(TokenType::Dot, self.start, self.current));
                        return int;
                    }
                }
                '0'..='9' => {}
                _ => return self.end_num(is_float),
            }
            self.pop();
        }
    }

    fn end_num(&mut self, is_float: bool) -> Token<'p> {
        let text = &self.src.source_slice(self.root)[self.start..self.current];
        if is_float {
            let n = match text.parse::<f64>() {
                Ok(n) => n,
                Err(_) => return self.err(LexErr::NumParseErr),
            };
            return self.token(Float(n), self.start, self.current);
        } else {
            let n = match text.parse::<i64>() {
                Ok(n) => n,
                Err(_) => return self.err(LexErr::NumParseErr),
            };
            return self.token(Number(n), self.start, self.current);
        }
    }

    fn lex_ident(&mut self) -> Token<'p> {
        self.pop();
        let mut c = self.peek_c();
        // TODO: only sometimes allow #
        while c.is_ascii_alphanumeric() || c == '_' || (!c.is_ascii() && !c.is_whitespace()) {
            self.pop();
            c = self.peek_c();
        }
        let ty = match &self.src.source_slice(self.root)[self.start..self.current] {
            "fn" => Fn,
            "let" => Qualifier(VarType::Let),
            "var" => Qualifier(VarType::Var),
            "const" => Qualifier(VarType::Const),
            text => Symbol(self.pool.intern(text)),
        };
        self.token(ty, self.start, self.current)
    }

    fn eat_whitespace(&mut self) {
        loop {
            // TODO count blank as comment
            // let mut first = true;
            while self.peek_c().is_whitespace() {
                if self.pop() == '\n' {
                    // if !first {
                    // todo!("untested");
                    // self.skipped_lines += 1;
                    // first = false;
                    // }
                }
            }
            if self.peek_c() == '/' {
                self.eat_comment();
                continue;
            }
            break;
        }
    }

    fn eat_comment(&mut self) {
        let c = self.pop(); // NOT IN THE debug_assert_eq DUMBASS (me)
        debug_assert_eq!(c, '/');
        match self.pop() {
            '/' => loop {
                match self.pop() {
                    '\0' => break,
                    '\n' => {
                        break;
                    }
                    _ => {}
                }
            },
            '*' => {
                let mut depth = 1;
                while depth > 0 {
                    match self.pop() {
                        '\0' => {
                            self.peeked.push_back(self.err(LexErr::Unexpected('\0')));
                            break;
                        }
                        '*' => {
                            if self.peek_c() == '/' {
                                self.pop();
                                depth -= 1;
                            }
                        }
                        '/' => {
                            if self.peek_c() == '*' {
                                self.pop();
                                depth += 1;
                            }
                        }
                        '\n' => {}
                        _ => {}
                    }
                }
            }
            c => self.peeked.push_back(self.err(LexErr::Unexpected(c))),
        }
    }

    fn lex_bin(&mut self) -> Token<'p> {
        let mut bit_count = 0;
        let mut value = 0;

        while let '0' | '1' = self.peek_c() {
            if bit_count == 64 {
                return self.err(LexErr::TooManyBits);
            }
            value <<= 1;
            value += self.pop() as u64 - '0' as u64;
            bit_count += 1;
        }

        self.token(TokenType::BinaryNum { bit_count, value }, self.start, self.current)
    }

    fn lex_hex(&mut self) -> Token<'p> {
        let mut bit_count = 0;
        let mut value = 0;

        loop {
            match self.peek_c() {
                '0'..='9' => {
                    if bit_count == 64 {
                        return self.err(LexErr::TooManyBits);
                    }
                    value *= 16;
                    value += self.pop() as u64 - '0' as u64;
                }
                'A'..='F' => {
                    if bit_count == 64 {
                        return self.err(LexErr::TooManyBits);
                    }
                    value *= 16;
                    value += self.pop() as u64 - 'A' as u64 + 10;
                }
                'a'..='f' => {
                    if bit_count == 64 {
                        return self.err(LexErr::TooManyBits);
                    }
                    value *= 16;
                    value += self.pop() as u64 - 'a' as u64 + 10;
                }
                _ => break,
            }
            bit_count += 4;
        }

        self.token(TokenType::BinaryNum { bit_count, value }, self.start, self.current)
    }

    fn peek_c(&mut self) -> char {
        match self.chars.peek() {
            None => 0 as char,
            Some(c) => *c,
        }
    }

    fn pop(&mut self) -> char {
        match self.chars.next() {
            None => 0 as char,
            Some(c) => {
                self.current += c.len_utf8();
                c
            }
        }
    }

    fn err(&self, err: LexErr) -> Token<'p> {
        self.token(TokenType::Error(err), self.start, self.current)
    }
}

// my language needs to have @using on a field or something
impl<'p> Deref for Token<'p> {
    type Target = TokenType<'p>;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}
