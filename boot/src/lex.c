
typedef void StringPool;  // TODO

typedef struct {
    u8 bit_count; i64 value;
} BinNum;
typedef enum {
    Plus, Minus, Star, Slash, Less, Greater, Bang, PlusEq, MinusEq, StarEq, SlashEq, LessEq, GreaterEq, BangEq, EqEq, AmpAmp, PipePipe,
} Operator;
typedef struct { u32 id; } Symbol;
typedef union {
    i64 Number;
    struct { u32 s; u8 escapes; } Quoted;
    Symbol Symbol;
    Operator Op;
    i64 Error;
    double Float;
    BinNum BinaryNum;
    i64 _LexErr[2];
} TokenPayload;
typedef enum {
    Number,
    Quoted,
    symbol,
    Op,
    _Error,
    Float,
    BinaryNum,
    LeftSquiggle, RightSquiggle, LeftParen, RightParen, LeftSquare, RightSquare,
    Dot, At, Comma, Colon, Semicolon, DoubleSquare, Hash, FatRightArrow, DoubleColon, 
    Amp, Question, Pipe, Equals, Dollar, Eof, Squiggle, 
    Fn,
    Tick, DotDot,
} TokenTag;
typedef struct { int low, high; } Span;
typedef struct {
    TokenTag tag;
    // TODO: zero the alignment padding here
    TokenPayload payload;
    Span span;
} Token;

static struct {
    StringPool *pool;
    Span root;
    Str src;
    i64 start;
    i64 current;
    Token token;
    i64 line;
    i64 old_line;
} lexer;

#define self lexer

int inclusive(u8 it, u8 lo, u8 hi) { return lo <= it && it <= hi; }

u8 lex_peek_n(i64 n) { return self.current + n < self.src.len ? self.src.ptr[self.current + n] : 0; }
u8 lex_peek_c() { return lex_peek_n(0); }

Span lex_subspan() {
    return (Span) { 
        .low = self.root.low + self.start, 
        .high = self.root.low + self.current,
    };
}

void lex_put_token(Token t) {
    if (self.current < self.src.len) self.current = self.src.len;
    t.span = lex_subspan();
    self.token = t;
}

void lex_one(TokenTag type) {
    self.current++;
    lex_put_token((Token) { .tag = type, });
}

Token *lex_peek() { return &self.token; }

void lex_skip_whitespace() {
    while(1) switch (lex_peek_c()) {
        case '\n': self.line += 1;  // fallthrough
        case ' ': case '\t': case '\r': self.current++; break;
        case '/': switch (lex_peek_n(1)) {
            case '/': {
                int c = 0;
                do {
                    self.current++;
                    c = lex_peek_c();
                } while (c != 0 && c != '\n');
                self.current++;
                self.line++;
            };
            break; 
            case '*': {
                i64 depth = 1;
                self.current += 2;
                while (depth > 0) {
                    switch (lex_peek_c()) {
                        case '/': if (lex_peek_n(1) == '*') {
                            depth++;
                            self.current++;
                        };
                        break;
                        case '*': if (lex_peek_n(1) == '/') {
                            depth--;
                            self.current++;
                        };
                        break;
                        case '\n': self.line++; break;
                        case 0: panic("unterminated comment");
                        default: break;
                    };
                    self.current += 1;
                };
            };
            break;
            default: return;
        };
        default: return;
    };
};

bool eat_white_space_hit_end() {
    lex_skip_whitespace();
    int end = self.current >= self.src.len;
    if (end) {
        self.start = self.current;
        lex_one(Eof);
    };
    return end;
}

void lex_string() {} // TODO
void lex_ident() {} // TODO

Token *lex_pop() {
    self.old_line = self.line;
    if (eat_white_space_hit_end()) return &self.token;
    self.start = self.current;
    
    u8 c = lex_peek_c();
    if (inclusive(c, 'a', 'z') || inclusive(c, 'A', 'Z') || c == '_') lex_ident();
    else switch (c) {
        case ';': lex_one(Semicolon); break;
        /*
        case ':': lex_pair(':', Colon, DoubleColon); break;
        case '.': lex_pair('.', Dot, Dot); break;
        */
        case '(': lex_one(LeftParen); break;
        case ')': lex_one(RightParen); break;
        case '}': lex_one(LeftSquiggle); break;
        case '{': lex_one(RightSquiggle); break;
        case ',': lex_one(Comma); break;
        case '$': lex_one(Dollar); break;
        /*
        case '+': lex_op_pair('=', Plus, PlusEq); break;
        case '-': lex_op_pair('=', Minus, MinusEq); break;
        case '*': lex_op_pair('=', Star, StarEq); break;
        case '/': lex_op_pair('=', Slash, SlashEq); break;
        case '<': lex_op_pair('=', Less, LessEq); break;
        case '>': lex_op_pair('=', Greater, GreaterEq); break;
        case '!': lex_op_pair('=', Bang, BangEq); break;
        */
        case '~': lex_one(Squiggle); break;
        /*
        @case("=".ascii()) => {
            found := @switch(self.peek_c(1)) {
                @case("=".ascii()) => {
                    self.current += 1;
                    @as(TokenType) (Op = .EqEq)
                };
                @case(">".ascii()) => {
                    self.current += 1;
                    TokenType.FatRightArrow
                };
                @default => TokenType.Equals;
            };
            self.one(found)
        };
        @case("@".ascii()) => {
            self.current += 1;
            if self.peek_c() == "\"".ascii() {
                self.start += 1;  // lex_string will skip the " but we have to skip the @
                if self.lex_string() {
                    s := self.token.type&;
                    if s.Quoted.escapes {
                        // TODO: expand_string_escapes
                        self.error(.TodoEscapesInRawIdent);
                    } else {
                        s[] = (Symbol = s.Quoted.s);
                    };
                };
                self.token.span.low -= 1;
            } else {
                self.put_token(.At);
            };
        };
        @case("&".ascii()) => self.pair("&".ascii(), .Amp, (Op = .AmpAmp));
        @case("#".ascii()) => {
            if self.peek_c(1) == "!".ascii() {
                dowhile() {
                    self.current += 1;
                    self.peek_c() != "\n".ascii() && self.peek_c() != 0
                };
                return(self.pop());
            };
            self.one(.Hash)
        };
        // 
        @case("?".ascii()) => self.one(.Question);
        @case("|".ascii()) => self.pair("|".ascii(), .Pipe, (Op = .PipePipe));
        case '[': lex_pair(']', LeftSquare, DoubleSquare); break;
        */
        case ']': lex_one(RightSquare); break;
        case '\'': lex_one(Tick); break;
        case '"': lex_string(); break;
        /*
        @case("0".ascii()) => {
            @switch(self.peek_c(1)) {
                @case("x".ascii()) => unimplimented("lex_hex");
                @case("b".ascii()) => unimplimented("lex_bin");
                @case(".".ascii()) => self.lex_num();
                @inclusive("0".ascii(), "9".ascii()) => panic("DenyLeadingZero");
                @default => self.one((Number = 0));
            };
        };
        @inclusive("1".ascii(), "9".ascii()) => self.lex_num();
        */
        case 0: lex_one(Eof); break;
        default: panic("unexpected character");
    };
    return &self.token;
}

/*
fn skip_to_closing_squigle(self: *Lexer) ?Ty(Str, Span) = {
    start := self.start;
    depth := 1;
    whileb (=> depth.gt(0)) { ($break) |
        if (self.eat_white_space_hit_end()) panic("unterminated block");
        switch (lex_peek_c()) {
            @case("\"".ascii()) => {
                self.skip_string();
            };
            case '{': depth += 2;  // fallthrough
            case '}': depth--; // fallthrough
            default: self.current++;
        };
    };
    text := self.src.slice(start, self.current);
    span := self.root.subspan(start.trunc(), self.current.trunc());
    self.start = self.current;
    self.peek();
    (Some = (text, span))
}

fn skip_string(self: *Lexer) Ty(Str, bool, bool) = {
    err :: fn() Never => return("FAILED TO LEX STRING YOU SHOULD NEVER SEE THIS ICE", false, false);
    
    is_multiline := self.peek_c(1).eq("\"".ascii()).and(=> self.peek_c(2).eq("\"".ascii()));
    escapes := false;
    text := if is_multiline {
        self.current += 2;
        dowhile {
            self.current += 1;
            @switch(self.peek_c()) {
                @case("\"".ascii()) => {
                    self.peek_c(1).eq("\"".ascii()).and(=> self.peek_c(2).eq("\"".ascii())).not()
                };
                case 0: panic("unterminated string literal");
                @default => true;
            }
        };
        self.current += 3;
        self.src.slice(self.start.add(3), self.current.sub(3))
    } {
        dowhile {
            self.current += 1;
            @switch(self.peek_c()) {
                @case("\"".ascii()) => false;
                @case("\\".ascii()) => {
                    self.current += 1; // extra for "\""
                    escapes = true;
                    true
                };
                @case("\n".ascii()) => {
                    self.error(.UnterminatedStr);
                    err();
                    false
                };
                @case(0) => {
                    self.error(.UnterminatedStr);
                    err();
                    false
                };
                @default => true;
            }
        };
        self.current += 1;
        self.src.slice(self.start.add(1), self.current.sub(1))
    };

    (text, escapes, true)
}

fn lex_string(self: *Lexer) bool = {
    // TODO: destructuring 
    text, escapes, success := self.skip_string();
    if success {
        self.put_token((Quoted = (s = self.pool.insert_owned(text), escapes = escapes)));
    };
    success
}

fn lex_num(self: *Lexer) void = {
    whole := self.lex_int();
    if self.peek_c() == ".".ascii() && self.peek_c(1).is_ascii_digit() {
        // Actually, that's a float.
        self.current += 1;
        start := self.current;
        fraction := self.lex_int();
        end := self.current;
        digits := end.sub(start);
        scale := 10.pow(digits);
        n := whole.float().add(fraction.float().div(scale.float()));
        self.put_token((Float = n));
    } else {
        self.put_token((Number = whole));
    };
}
*/

i64 lex_int() { 
    i64 total = 0;
    do {
        total *= 10;
        total += lex_peek_c() - '0';
        self.current++;
        while (lex_peek_c() == '_') self.current++;
    } while ('0' <= lex_peek_c() && lex_peek_c() <= '9');
    return total;
}

int is_hex_digit(u8 it) { return inclusive(it, '0', '9') || inclusive(it, 'a', 'f') || inclusive(it, 'A', 'F'); }

/*
fn lex_ident(self: *Lexer) void = {
    dowhile {
        self.current += 1;
        c := self.peek_c();
        c.is_ascii_alpha() || c.is_ascii_digit() || c == "_".ascii()
    };
    name := self.src.slice(self.start, self.current);
    if name.len == 2 && name[0] == "f".ascii() && name[1] == "n".ascii() {
        self.put_token(.Fn);
    } else {
        ident := self.pool.insert_owned(name);
        self.put_token((Symbol = ident))
    };
}

::if(TokenType);
fn pair(self: *Lexer, maybe: u8, single: TokenType, double: TokenType) void = {
    t := if(self.peek_c(1) == maybe, => {
        self.current += 1;
        double
    }, => single);
    self.one(t);
}
*/

/*
fn init(pool: *StringPool, root: Span, src: Str) Lexer = {
    self: Lexer = (pool = pool, root = root, src = src, token = Token.zeroed());
    self&.pop(); // get the first token ready. 
    self
}
*/

#undef self
