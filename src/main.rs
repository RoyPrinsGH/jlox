use std::{
    fs::read_to_string,
    io::{Write, stdin, stdout},
    iter::{self},
    path::{Path, PathBuf},
};

use ariadne::{Cache, Color, Label, Report, ReportKind, Source, Span};
use clap::Parser;
use miette::{Context, IntoDiagnostic, Result, miette};

#[derive(Parser, Debug)]
struct Args {
    /// Name of the person to greet
    script: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();

    match args.script {
        Some(script_path) => run_script(script_path),
        None => run_prompt(),
    }
}

fn run_script(script_path: impl AsRef<Path>) -> Result<()> {
    let script_path = script_path.as_ref();

    let script_content = read_to_string(script_path)
        .into_diagnostic()
        .wrap_err(miette!("Specified script {:?} not found", script_path))?;

    run(script_content)
}

fn run_prompt() -> Result<()> {
    let mut line_to_execute = String::new();

    loop {
        print!("[Jlox REPL] >> ");

        stdout()
            .flush()
            .into_diagnostic()
            .wrap_err(miette!("Failed to flush stdout: {:?}", line_to_execute))?;

        stdin()
            .read_line(&mut line_to_execute)
            .into_diagnostic()
            .wrap_err(miette!("Failed to read from stdin: {:?}", line_to_execute))?;

        run(&line_to_execute)?;

        line_to_execute.clear();
    }
}

#[derive(Debug)]
enum Token<'s> {
    // Single-character tokens.
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Asterisk,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier,
    String(&'s str),
    Number(f64),
    // Keywords.
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
enum LexerError {
    UnrecognisedToken,
    UnfinishedString,
    InvalidNumber,
}

#[derive(Debug)]
struct TokenSpan {
    start: usize,
    end: usize,
}

#[derive(Clone, Copy, Debug)]
enum LexerScope {
    None,
    Unrecognised,
    String,
    Number,
}

fn lex<'a>(script: &'a str) -> impl Iterator<Item = Result<Token<'a>, (TokenSpan, LexerError)>> {
    let mut scope = (0, LexerScope::None);
    let mut chars = script.char_indices().fuse().peekable();
    let mut to_yield = Vec::new();
    let mut exit = false;

    iter::from_fn(move || {
        if let Some(item) = to_yield.pop() {
            return Some(item);
        }

        if exit {
            return None;
        }

        loop {
            match (scope, chars.next(), chars.peek()) {
                ((_, LexerScope::None), Some((ix, c)), maybe_nc) => {
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
                        '\n' | ' ' => {
                            continue;
                        }
                        '\"' => {
                            scope = (ix, LexerScope::String);
                            continue;
                        }
                        c if c.is_ascii_digit() => {
                            if let Some((_, nc)) = maybe_nc
                                && nc.is_ascii_digit()
                            {
                                scope = (ix, LexerScope::Number);
                                continue;
                            } else {
                                Token::Number((c as u8 - '0' as u8) as f64)
                            }
                        }
                        _ => {
                            scope = (ix, LexerScope::Unrecognised);
                            continue;
                        }
                    };

                    to_yield.push(Ok(token));
                }
                ((_, LexerScope::None), None, _) => {
                    exit = true;
                }
                ((start, LexerScope::String), Some((ix, c)), _) => match c {
                    '\"' => {
                        to_yield.push(Ok(Token::String(&script[start + 1..ix])));
                        scope = (ix, LexerScope::None)
                    }
                    '\\' => {
                        // Escape char: Advance without looking
                        _ = chars.next();
                        continue;
                    }
                    _ => {
                        continue;
                    }
                },
                ((start, LexerScope::String), None, _) => {
                    to_yield.push(Err((
                        TokenSpan {
                            start,
                            end: script.len(),
                        },
                        LexerError::UnfinishedString,
                    )));
                    exit = true;
                }
                ((start, LexerScope::Number), _, Some((nc_ix, nc))) => {
                    if nc.is_ascii_digit() || *nc == '.' {
                        continue;
                    } else {
                        let result = (&script[start..*nc_ix])
                            .parse()
                            .map(|float| Token::Number(float))
                            .map_err(|_| {
                                (TokenSpan { start, end: *nc_ix }, LexerError::InvalidNumber)
                            });

                        to_yield.push(result);
                        scope = (*nc_ix, LexerScope::None);
                    }
                }
                ((start, LexerScope::Number), _, None) => {
                    let result = (&script[start..])
                        .parse()
                        .map(|float| Token::Number(float))
                        .map_err(|_| {
                            (
                                TokenSpan {
                                    start,
                                    end: script.len(),
                                },
                                LexerError::InvalidNumber,
                            )
                        });

                    to_yield.push(result);
                    exit = true;
                }
                ((start, LexerScope::Unrecognised), Some((ix, c)), _) => match c {
                    ' ' => {
                        to_yield.push(Err((
                            TokenSpan { start, end: ix },
                            LexerError::UnrecognisedToken,
                        )));
                        scope = (ix, LexerScope::None);
                    }
                    _ => continue,
                },
                ((start, LexerScope::Unrecognised), None, _) => {
                    to_yield.push(Err((
                        TokenSpan {
                            start,
                            end: script.len(),
                        },
                        LexerError::UnrecognisedToken,
                    )));
                    exit = true;
                }
            }
            break;
        }

        to_yield.pop()
    })
}

fn report_lexer_error(script_name: &str, script_source: &str, span: TokenSpan, error: LexerError) {
    let mut report_builder = Report::build(ReportKind::Error, (script_name, span.start..span.end));

    match error {
        LexerError::UnfinishedString => {
            report_builder = report_builder
                .with_code(1)
                .with_message("Unfinished string")
                .with_label(
                    Label::new((script_name, span.end..span.end))
                        .with_message("Expected end of string here")
                        .with_color(Color::Red),
                )
        }
        LexerError::UnrecognisedToken => {
            report_builder = report_builder
                .with_code(2)
                .with_message("Unrecognised token")
                .with_label(
                    Label::new((script_name, span.start..span.end))
                        .with_message("What does this mean?")
                        .with_color(Color::Red),
                )
        }
        LexerError::InvalidNumber => {
            report_builder = report_builder
                .with_code(3)
                .with_message("Invalid number")
                .with_label(
                    Label::new((script_name, span.start..span.end))
                        .with_message("This does not parse to a f64")
                        .with_color(Color::Red),
                )
        }
    }

    report_builder
        .finish()
        .eprint((script_name, Source::from(script_source)))
        .into_diagnostic()
        .wrap_err(miette!("Failed to print compiler error to stdout"))
        .expect("stdout available")
}

fn run(script: impl AsRef<str>) -> Result<()> {
    let script = script.as_ref().trim();

    for restok in lex(script) {
        match restok {
            Ok(token) => println!("{token:?}"),
            Err((span, err)) => report_lexer_error("repl", script, span, err),
        }
    }

    Ok(())
}
