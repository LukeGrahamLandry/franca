//! Convert a string of source code into a stream of tokens.

use codemap::Span;

use crate::ast::VarType;
use crate::lex::TokenType::*;
use crate::pool::{Ident, StringPool};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::ops::Deref;
use std::str::Chars;

// TODO: why tf is Range not copy
#[derive(Debug, Clone)]
pub struct Token<'p> {
    pub kind: TokenType<'p>,
    pub span: Span,
}

// TODO: true/false
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType<'p> {
    Symbol(Ident<'p>),
    Number(i64),
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
    BinaryNum { bit_count: u8, value: u128 },
    LeftArrow,
    Error(LexErr),
}

pub type Res<T> = Result<T, LexErr>;

#[derive(Debug, Clone, Copy, PartialEq)]
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
    src: &'a str,
    start: usize,
    current: usize,
    chars: Peekable<Chars<'a>>,
    peeked: VecDeque<Token<'p>>,
}

impl<'a, 'p> Lexer<'a, 'p> {
    pub fn new(src: &'a str, pool: &'p StringPool<'p>, root: Span) -> Self {
        Self {
            src,
            start: 0,
            current: 0,
            chars: src.chars().peekable(),
            peeked: VecDeque::with_capacity(10),
            root,
            pool,
        }
    }

    // pop the first buffered token or generate a new one
    #[allow(clippy::should_implement_trait)] // nobody cares bro
    pub fn next(&mut self) -> Token<'p> {
        if let Some(prev) = self.peeked.pop_front() {
            return prev;
        }

        self.do_next()
    }

    fn do_next(&mut self) -> Token<'p> {
        self.eat_whitespace();
        self.start = self.current;
        match self.peek_c() {
            '\0' => self.one(TokenType::Eof),
            '"' | '“' | '”' | '\'' => self.lex_quoted(),
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
                    let text = &self.src[self.start + 1..self.current - 1];
                    return self.token(Quoted(self.pool.intern(text)), self.start, self.current);
                }
                // I could let you have multi-line, but I don't like it because you end up with garbage indentation.
                '\n' | '\0' => return self.err(LexErr::UnterminatedStr),
                _ => {}
            }
        }
    }

    fn lex_num(&mut self) -> Token<'p> {
        loop {
            let done = match self.peek_c() {
                '\0' => true,
                c => !c.is_numeric() && c != '.',
            };
            if done {
                let text = &self.src[self.start..self.current];
                let n = match text.parse::<i64>() {
                    Ok(n) => n,
                    Err(_) => return self.err(LexErr::NumParseErr),
                };
                return self.token(Number(n), self.start, self.current);
            }
            self.pop();
        }
    }

    fn lex_ident(&mut self) -> Token<'p> {
        let mut c = self.peek_c();
        while c.is_ascii_alphanumeric() || c == '_' {
            self.pop();
            c = self.peek_c();
        }
        let ty = match &self.src[self.start..self.current] {
            "fn" => Fn,
            "fun" => Fun,
            "let" => Qualifier(VarType::Let),
            "var" => Qualifier(VarType::Var),
            "const" => Qualifier(VarType::Const),
            text => Symbol(self.pool.intern(text)),
        };
        self.token(ty, self.start, self.current)
    }

    fn eat_whitespace(&mut self) {
        loop {
            while self.peek_c().is_whitespace() {
                self.pop();
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
                    '\0' | '\n' => break,
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
            if bit_count == 128 {
                return self.err(LexErr::TooManyBits);
            }
            value <<= 1;
            value += self.pop() as u128 - '0' as u128;
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
                    if bit_count == 128 {
                        return self.err(LexErr::TooManyBits);
                    }
                    value *= 16;
                    value += self.pop() as u128 - '0' as u128;
                }
                'A'..='F' => {
                    if bit_count == 128 {
                        return self.err(LexErr::TooManyBits);
                    }
                    value *= 16;
                    value += self.pop() as u128 - 'A' as u128 + 10;
                }
                'a'..='f' => {
                    if bit_count == 128 {
                        return self.err(LexErr::TooManyBits);
                    }
                    value *= 16;
                    value += self.pop() as u128 - 'a' as u128 + 10;
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
