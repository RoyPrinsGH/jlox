use std::{
    iter::{self},
    ops::Range,
};

#[derive(Debug, PartialEq)]
pub enum Token<'s> {
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,
    Bang,
    Equal,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Identifier(&'s str),
    String(&'s str),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF,
}

#[derive(Debug)]
pub enum LexerError {
    UnrecognisedToken,
    UnfinishedString,
    InvalidNumber,
}

#[derive(Clone, Copy, Debug)]
enum SpanKind {
    Unrecognised,
    String,
    Number,
    Identifier,
}

#[derive(Debug, Clone, Copy)]
struct Span {
    from: usize,
    kind: SpanKind,
}

#[derive(Debug)]
struct SpanTracker {
    span: Option<Span>,
}

impl SpanTracker {
    fn new() -> Self {
        Self { span: None }
    }

    fn track_new(&mut self, ix: usize, kind: SpanKind) {
        self.span = Some(Span { from: ix, kind })
    }

    fn reset(&mut self) {
        self.span = None;
    }
}

fn ident_to_token<'s>(ident: &'s str) -> Token<'s> {
    match ident {
        "and" => Token::And,
        "class" => Token::Class,
        "else" => Token::Else,
        "false" => Token::False,
        "for" => Token::For,
        "fun" => Token::Fun,
        "if" => Token::If,
        "nil" => Token::Nil,
        "or" => Token::Or,
        "print" => Token::Print,
        "return" => Token::Return,
        "super" => Token::Super,
        "this" => Token::This,
        "true" => Token::True,
        "var" => Token::Var,
        "while" => Token::While,
        non_reserved => Token::Identifier(non_reserved),
    }
}

pub fn lex<'a>(source: &'a str) -> impl Iterator<Item = Result<Token<'a>, (Range<usize>, LexerError)>> {
    let mut span_tracker = SpanTracker::new();
    let mut chars = source.char_indices().fuse().peekable();
    iter::from_fn(move || {
        let token_or_err = loop {
            match (span_tracker.span, chars.next(), chars.peek()) {
                (None, Some((ix, c)), maybe_nc) => {
                    let token = match c {
                        '(' => Token::LeftParenthesis,
                        ')' => Token::RightParenthesis,
                        '{' => Token::LeftBrace,
                        '}' => Token::RightBrace,
                        ',' => Token::Comma,
                        '.' => Token::Dot,
                        '-' => Token::Minus,
                        '+' => Token::Plus,
                        ';' => Token::Semicolon,
                        '*' => Token::Asterisk,
                        '!' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                Token::BangEqual
                            }
                            _ => Token::Bang,
                        },
                        '=' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                Token::EqualEqual
                            }
                            _ => Token::Equal,
                        },
                        '<' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                Token::LessEqual
                            }
                            _ => Token::Less,
                        },
                        '>' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                Token::GreaterEqual
                            }
                            _ => Token::Greater,
                        },
                        '/' => match maybe_nc {
                            Some((_, '/')) => {
                                _ = chars.next();
                                while let Some((_, nc)) = chars.next() {
                                    match nc {
                                        '\n' => {
                                            break;
                                        }
                                        _ => {
                                            continue;
                                        }
                                    }
                                }
                                continue;
                            }
                            _ => Token::Slash,
                        },
                        '\n' | ' ' => continue,
                        '\"' => {
                            span_tracker.track_new(ix, SpanKind::String);
                            continue;
                        }
                        c if c.is_ascii_digit() => {
                            if let Some((_, nc)) = maybe_nc
                                && (nc.is_ascii_digit() || *nc == '.')
                            {
                                span_tracker.track_new(ix, SpanKind::Number);
                                continue;
                            } else {
                                Token::Number((c as u8 - '0' as u8) as f64)
                            }
                        }
                        c if c.is_alphabetic() => {
                            if let Some((_, nc)) = maybe_nc
                                && nc.is_alphanumeric()
                            {
                                span_tracker.track_new(ix, SpanKind::Identifier);
                                continue;
                            } else {
                                ident_to_token(&source[ix..=ix])
                            }
                        }
                        _ => {
                            span_tracker.track_new(ix, SpanKind::Unrecognised);
                            continue;
                        }
                    };

                    break Ok(token);
                }
                (None, None, _) => return None,
                (Some(Span { from, kind: SpanKind::String }), Some((ix, c)), _) => match c {
                    '\"' => {
                        span_tracker.reset();
                        break Ok(Token::String(&source[from + 1..ix]));
                    }
                    '\\' => {
                        // Escape char: Advance without looking
                        _ = chars.next();
                        continue;
                    }
                    _ => continue,
                },
                (Some(Span { from, kind: SpanKind::String }), None, _) => {
                    span_tracker.reset();
                    break Err((from..source.len(), LexerError::UnfinishedString));
                }
                (Some(Span { from, kind: SpanKind::Number }), _, Some((nc_ix, nc))) => {
                    if nc.is_ascii_digit() || *nc == '.' {
                        continue;
                    } else {
                        span_tracker.reset();
                        break (&source[from..*nc_ix])
                            .parse()
                            .map(|float| Token::Number(float))
                            .map_err(|_| (from..*nc_ix, LexerError::InvalidNumber));
                    }
                }
                (Some(Span { from, kind: SpanKind::Number }), _, None) => {
                    span_tracker.reset();
                    break (&source[from..])
                        .parse()
                        .map(|float| Token::Number(float))
                        .map_err(|_| ((from..source.len()), LexerError::InvalidNumber));
                }
                (Some(Span { from, kind: SpanKind::Unrecognised }), Some((ix, c)), _) => match c {
                    ' ' => {
                        span_tracker.reset();
                        break Err(((from..ix), LexerError::UnrecognisedToken));
                    }
                    _ => continue,
                },
                (Some(Span { from, kind: SpanKind::Unrecognised }), None, _) => {
                    span_tracker.reset();
                    break Err(((from..source.len()), LexerError::UnrecognisedToken));
                }
                (Some(Span { from, kind: SpanKind::Identifier }), _, Some((nc_ix, nc))) => {
                    if nc.is_alphanumeric() {
                        continue;
                    } else {
                        span_tracker.reset();
                        break Ok(ident_to_token(&source[from..*nc_ix]));
                    }
                }
                (Some(Span { from, kind: SpanKind::Identifier }), _, None) => {
                    span_tracker.reset();
                    break Ok(ident_to_token(&source[from..]));
                }
            }
        };

        Some(token_or_err)
    })
    .chain(iter::once(Ok(Token::EOF)))
}
