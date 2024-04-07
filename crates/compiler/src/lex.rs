//! Convert a string of source code into a stream of tokens.
// TODO: ignore shabang line and make sure compiler supports running scripts that way. thats a good side affect of the "all info needed in program" goal.
// TODO: this could be a good thing to start with for trying self hosting.

use crate::ast::TypeId;
use crate::ast::VarType;
use crate::bc::Value;
use crate::ffi::InterpSend;
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
#[derive(Debug, Clone, InterpSend)]
pub struct Token<'p> {
    pub kind: TokenType<'p>,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, InterpSend)]
pub enum TokenType<'p> {
    Symbol(Ident<'p>),
    Number(i64),
    Float(f64),
    Quoted(Ident<'p>),
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
    Fun,
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
    DoubleSquare,
    DoubleSquigle,
    PlusEq,
    MinusEq,
    DoubleColon,
    Pipe,
    // Bit count means leading zeros are observable.
    BinaryNum { bit_count: u8, value: u64 },
    LeftArrow,
    IncludeStd,
    Directive(Ident<'p>),
    Error(LexErr),
}

#[derive(Debug, Clone, Copy, PartialEq, InterpSend)]
pub enum LexErr {
    UnterminatedStr,
    Unexpected(char),
    NumParseErr,
    /// C has it mean octal which I don't really like but I don't want to accept the same syntax and mean something different.
    DenyLeadingZero,
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
    pub(crate) line: usize,
    pub(crate) comment_lines: usize,
    hack: Option<Token<'p>>,
}

impl<'a, 'p> Lexer<'a, 'p> {
    pub fn new(src: Arc<File>, pool: &'p StringPool<'p>, root: Span) -> Self {
        // Safety: its in an arc which is dropped at the same time as the iterator.
        let hack = unsafe { &*(src.source() as *const str) };
        Self {
            src,
            start: 0,
            current: 0,
            peeked: VecDeque::with_capacity(10),
            chars: hack.chars().peekable(),
            root,
            pool,
            line: 0,
            comment_lines: 0,
            hack: None,
        }
    }

    // pop the first buffered token or generate a new one
    #[allow(clippy::should_implement_trait)] // nobody cares bro
    pub fn next(&mut self) -> Token<'p> {
        if let Some(prev) = self.peeked.pop_front() {
            return prev;
        }

        let t = self.do_next();
        if let Some(t) = self.hack.take() {
            self.peeked.push_back(t);
        }
        t
    }

    fn do_next(&mut self) -> Token<'p> {
        self.eat_whitespace();
        self.start = self.current;
        match self.peek_c() {
            '\0' => self.one(TokenType::Eof),
            '#' => self.lex_ident(),
            '"' | '“' | '”' | '\'' => self.lex_quoted(),
            '`' => self.lex_quoted_multiline(),
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
                    '0'..='9' => self.err(LexErr::DenyLeadingZero),
                    _ => self.token(Number(0), self.start, self.current),
                }
            }
            '1'..='9' => self.lex_num(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_ident(),
            '{' => self.pair('}', LeftSquiggle, DoubleSquigle),
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
            '=' => self.one(Equals),
            '*' => self.one(Star),
            '^' => self.one(UpArrow),
            '&' => self.one(Amp),
            '?' => self.one(Question),
            '|' => self.one(Pipe),
            '/' => self.one(Slash),
            '+' => self.pair('=', Error(LexErr::Unexpected('+')), PlusEq),
            '-' => self.pair('=', Error(LexErr::Unexpected('-')), MinusEq),
            '<' => self.pair('-', Error(LexErr::Unexpected('<')), LeftArrow),
            c => self.err(LexErr::Unexpected(c)),
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
        self.pop();
        loop {
            match self.pop() {
                '"' | '“' | '”' | '\'' => {
                    // Payload doesn't include quotes.
                    let text = &self.src.source()[self.start + 1..self.current - 1];
                    return self.token(Quoted(self.pool.intern(text)), self.start, self.current);
                }
                // I could let you have multi-line, but I don't like it because you end up with garbage indentation.
                '\n' | '\0' => return self.err(LexErr::UnterminatedStr),
                _ => {}
            }
        }
    }

    fn lex_quoted_multiline(&mut self) -> Token<'p> {
        self.pop();
        loop {
            match self.pop() {
                '`' => {
                    // Payload doesn't include quotes.
                    let text = &self.src.source()[self.start + 1..self.current - 1];
                    return self.token(Quoted(self.pool.intern(text)), self.start, self.current);
                }
                '\0' => return self.err(LexErr::UnterminatedStr),
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
        let text = &self.src.source()[self.start..self.current];
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
        let mut c = self.peek_c();
        let is_directive = c == '#';
        self.pop();
        c = self.peek_c();
        if is_directive && c == '!' {
            // https://en.wikipedia.org/wiki/Shebang_(Unix)
            while self.peek_c() != '\n' {
                self.pop();
            }
            // Just something random that's allowed at the start of the file
            return self.token(Semicolon, self.start, self.current);
        }
        // TODO: only sometimes allow #
        while c.is_ascii_alphanumeric() || c == '_' || c == '#' {
            self.pop();
            c = self.peek_c();
        }
        let ty = match &self.src.source()[self.start..self.current] {
            "fn" => Fn,
            "fun" => Fun,
            "let" => Qualifier(VarType::Let),
            "var" => Qualifier(VarType::Var),
            "const" => Qualifier(VarType::Const),
            "#include_std" => IncludeStd,
            text => {
                if is_directive {
                    Directive(self.pool.intern(&text[1..]))
                } else {
                    Symbol(self.pool.intern(text))
                }
            }
        };
        self.token(ty, self.start, self.current)
    }

    fn eat_whitespace(&mut self) {
        loop {
            // TODO count blank as comment
            while self.peek_c().is_whitespace() {
                if self.pop() == '\n' {
                    self.line += 1;
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
                        self.line += 1;
                        self.comment_lines += 1;
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
                        '\n' => {
                            self.line += 1;
                            self.comment_lines += 1;
                        }
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
