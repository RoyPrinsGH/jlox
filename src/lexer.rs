use std::{
    iter::{self},
    ops::Range,
};

#[derive(Debug)]
pub struct Token<'s> {
    pub data: TokenData<'s>,
    pub span: Range<usize>,
}

#[derive(Debug, PartialEq)]
pub enum TokenData<'s> {
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

#[derive(Clone, Copy, Debug, Default)]
enum SpanKind {
    #[default]
    None,
    Unrecognised,
    String,
    Number,
    Identifier,
}

#[derive(Debug, Clone, Copy, Default)]
struct SpanTracker {
    from: usize,
    kind: SpanKind,
}

fn ident_to_token<'s>(ident: &'s str) -> TokenData<'s> {
    match ident {
        "and" => TokenData::And,
        "class" => TokenData::Class,
        "else" => TokenData::Else,
        "false" => TokenData::False,
        "for" => TokenData::For,
        "fun" => TokenData::Fun,
        "if" => TokenData::If,
        "nil" => TokenData::Nil,
        "or" => TokenData::Or,
        "print" => TokenData::Print,
        "return" => TokenData::Return,
        "super" => TokenData::Super,
        "this" => TokenData::This,
        "true" => TokenData::True,
        "var" => TokenData::Var,
        "while" => TokenData::While,
        non_reserved => TokenData::Identifier(non_reserved),
    }
}

pub fn lex<'a>(source: &'a str) -> impl Iterator<Item = (Range<usize>, Result<TokenData<'a>, LexerError>)> {
    let mut span_tracker = SpanTracker::default();
    let mut chars = source.char_indices().fuse().peekable();
    iter::from_fn(move || {
        let token_or_err = loop {
            match (&mut span_tracker, chars.next(), chars.peek()) {
                (span @ SpanTracker { kind: SpanKind::None, .. }, Some((ix, c)), maybe_nc) => {
                    let token = match c {
                        '(' => TokenData::LeftParenthesis,
                        ')' => TokenData::RightParenthesis,
                        '{' => TokenData::LeftBrace,
                        '}' => TokenData::RightBrace,
                        ',' => TokenData::Comma,
                        '.' => TokenData::Dot,
                        '-' => TokenData::Minus,
                        '+' => TokenData::Plus,
                        ';' => TokenData::Semicolon,
                        '*' => TokenData::Asterisk,
                        '!' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                TokenData::BangEqual
                            }
                            _ => TokenData::Bang,
                        },
                        '=' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                TokenData::EqualEqual
                            }
                            _ => TokenData::Equal,
                        },
                        '<' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                TokenData::LessEqual
                            }
                            _ => TokenData::Less,
                        },
                        '>' => match maybe_nc {
                            Some((_, '=')) => {
                                _ = chars.next();
                                TokenData::GreaterEqual
                            }
                            _ => TokenData::Greater,
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
                            _ => TokenData::Slash,
                        },
                        '\n' | ' ' => continue,
                        '\"' => {
                            *span = SpanTracker { from: ix, kind: SpanKind::String };
                            continue;
                        }
                        c if c.is_ascii_digit() => {
                            if let Some((_, nc)) = maybe_nc
                                && (nc.is_ascii_digit() || *nc == '.')
                            {
                                *span = SpanTracker { from: ix, kind: SpanKind::Number };
                                continue;
                            } else {
                                TokenData::Number((c as u8 - '0' as u8) as f64)
                            }
                        }
                        c if c.is_alphabetic() => {
                            if let Some((_, nc)) = maybe_nc
                                && nc.is_alphanumeric()
                            {
                                *span = SpanTracker {
                                    from: ix,
                                    kind: SpanKind::Identifier,
                                };
                                continue;
                            } else {
                                ident_to_token(&source[ix..=ix])
                            }
                        }
                        _ => {
                            *span = SpanTracker {
                                from: ix,
                                kind: SpanKind::Unrecognised,
                            };
                            continue;
                        }
                    };

                    break Ok(token);
                }
                (SpanTracker { kind: SpanKind::None, .. }, None, _) => return None,
                (SpanTracker { from, kind: SpanKind::String }, Some((ix, c)), _) => match c {
                    '\"' => {
                        break Ok(TokenData::String(&source[*from + 1..ix]));
                    }
                    '\\' => {
                        // Escape char: Advance without looking
                        _ = chars.next();
                        continue;
                    }
                    _ => continue,
                },
                (SpanTracker { kind: SpanKind::String, .. }, None, _) => {
                    break Err(LexerError::UnfinishedString);
                }
                (SpanTracker { from, kind: SpanKind::Number }, _, Some((nc_ix, nc))) => {
                    if nc.is_ascii_digit() || *nc == '.' {
                        continue;
                    } else {
                        break (&source[*from..*nc_ix]).parse().map(|float| TokenData::Number(float)).map_err(|_| LexerError::InvalidNumber);
                    }
                }
                (SpanTracker { from, kind: SpanKind::Number }, _, None) => {
                    break (&source[*from..]).parse().map(|float| TokenData::Number(float)).map_err(|_| LexerError::InvalidNumber);
                }
                (SpanTracker { kind: SpanKind::Unrecognised, .. }, Some((_, c)), _) => match c {
                    ' ' => break Err(LexerError::UnrecognisedToken),
                    _ => continue,
                },
                (SpanTracker { kind: SpanKind::Unrecognised, .. }, None, _) => break Err(LexerError::UnrecognisedToken),
                (SpanTracker { from, kind: SpanKind::Identifier }, _, Some((nc_ix, nc))) => {
                    if nc.is_alphanumeric() {
                        continue;
                    } else {
                        break Ok(ident_to_token(&source[*from..*nc_ix]));
                    }
                }
                (SpanTracker { from, kind: SpanKind::Identifier }, _, None) => break Ok(ident_to_token(&source[*from..])),
            }
        };

        let span = match chars.peek() {
            Some((ncix, _)) => span_tracker.from..*ncix,
            None => span_tracker.from..source.len(),
        };

        span_tracker = SpanTracker {
            from: span.end + 1,
            kind: SpanKind::None,
        };

        Some((span, token_or_err))
    })
    .chain(iter::once((source.len()..source.len(), Ok(TokenData::EOF))))
}
