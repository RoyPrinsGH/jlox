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

#[derive(Clone, Copy, Debug)]
struct Position {
    line: usize,
    index_in_line: usize,
    index: usize,
}

impl Position {
    fn new_line(&mut self) {
        self.line += 1;
        self.index_in_line = 0;
        self.index += 1;
    }

    fn further(&mut self) {
        self.index_in_line += 1;
        self.index += 1;
    }
}

#[derive(Debug)]
enum LexerError {
    UnrecognisedToken,
    UnfinishedString,
    UnterminatedNumber,
    InvalidNumber,
}

#[derive(Debug)]
struct TokenSpan {
    start: Position,
    end: Position,
}

enum LexerScope {
    Unrecognised,
    String,
    Number,
}

fn lex<'a>(script: &'a str) -> impl Iterator<Item = Result<Token<'a>, (TokenSpan, LexerError)>> {
    let mut current_position = Position {
        line: 0,
        index_in_line: 0,
        index: 0,
    };

    let mut next_position = Position {
        line: 0,
        index_in_line: 0,
        index: 0,
    };

    let mut scope = None;
    let mut script_char_indices = script.chars().peekable().fuse();

    let mut next_iter_yields = vec![];

    iter::from_fn(move || {
        if let Some(item) = next_iter_yields.pop() {
            return item;
        }

        loop {
            current_position = next_position;
            next_position.further();

            let Some(character) = script_char_indices.next() else {
                let (start, scope_type) = scope.take()?;

                let err = match scope_type {
                    LexerScope::Unrecognised => LexerError::UnrecognisedToken,
                    LexerScope::String => LexerError::UnfinishedString,
                    LexerScope::Number => LexerError::UnterminatedNumber,
                };

                next_iter_yields.push(None);

                break Some(Err((
                    TokenSpan {
                        start,
                        end: current_position,
                    },
                    err,
                )));
            };

            if let Some((start, LexerScope::String)) = scope {
                if character == '\"' {
                    scope = None;
                    break Some(Ok(Token::String(
                        str::from_utf8(&script.as_bytes()[start.index + 1..current_position.index])
                            .expect("script is utf8"),
                    )));
                } else if character == '\\' {
                    let _ = script_char_indices.next();
                    continue;
                } else {
                    continue;
                }
            }

            if let Some((start, LexerScope::Number)) = scope {
                if character == '.' {
                    continue;
                } else if !character.is_ascii_digit() {
                    scope = None;
                    match str::from_utf8(&script.as_bytes()[start.index..current_position.index])
                        .expect("script is utf8")
                        .parse()
                    {
                        Ok(float) => break Some(Ok(Token::Number(float))),
                        Err(_) => {
                            break Some(Err((
                                TokenSpan {
                                    start,
                                    end: current_position,
                                },
                                LexerError::InvalidNumber,
                            )));
                        }
                    }
                } else {
                    continue;
                }
            }

            let token = match character {
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
                ' ' => {
                    continue;
                }
                '\n' => {
                    current_position.new_line();
                    continue;
                }
                '\"' => {
                    if !matches!(&scope, Some((_, LexerScope::String))) {
                        scope = Some((current_position, LexerScope::String));
                    }
                    continue;
                }
                c if c.is_ascii_digit() => {
                    if !matches!(&scope, Some((_, LexerScope::Number))) {
                        scope = Some((current_position, LexerScope::Number));
                    }
                    continue;
                }
                _ => {
                    if !matches!(&scope, Some((_, LexerScope::Unrecognised))) {
                        scope = Some((current_position, LexerScope::Unrecognised));
                    }
                    continue;
                }
            };

            if let Some((start, scope_type)) = scope.take() {
                let err = match scope_type {
                    LexerScope::Unrecognised => LexerError::UnrecognisedToken,
                    LexerScope::String => LexerError::UnfinishedString,
                    LexerScope::Number => LexerError::UnterminatedNumber,
                };

                next_iter_yields.push(Some(Ok(token)));

                break Some(Err((
                    TokenSpan {
                        start,
                        end: current_position,
                    },
                    err,
                )));
            }

            break Some(Ok(token));
        }
    })
}

fn report_lexer_error(script_source: &str, span: TokenSpan, error: LexerError) {
    let mut report_builder = Report::build(ReportKind::Error, span.start.index..span.end.index);

    match error {
        LexerError::UnfinishedString => {
            report_builder = report_builder
                .with_code(1)
                .with_message("Unfinished string")
                .with_label(
                    Label::new(span.end.index..span.end.index)
                        .with_message("Expected end of string here")
                        .with_color(Color::Red),
                )
        }
        LexerError::UnrecognisedToken => {
            report_builder = report_builder
                .with_code(2)
                .with_message("Unrecognised token")
                .with_label(
                    Label::new(span.start.index..span.end.index)
                        .with_message("What does this mean?")
                        .with_color(Color::Red),
                )
        }
        LexerError::UnterminatedNumber => {
            report_builder = report_builder
                .with_code(3)
                .with_message("Unterminated number")
                .with_label(
                    Label::new(span.end.index..span.end.index)
                        .with_message("Expected end of number here")
                        .with_color(Color::Red),
                )
        }
        LexerError::InvalidNumber => {
            report_builder = report_builder
                .with_code(3)
                .with_message("Invalid number")
                .with_label(
                    Label::new(span.start.index..span.end.index)
                        .with_message("This does not parse to a f64")
                        .with_color(Color::Red),
                )
        }
    }

    report_builder
        .finish()
        .eprint(Source::from(script_source))
        .into_diagnostic()
        .wrap_err(miette!("Failed to print compiler error to stdout"))
        .expect("stdout available")
}

fn run(script: impl AsRef<str>) -> Result<()> {
    let script = script.as_ref().trim();

    for restok in lex(script) {
        match restok {
            Ok(token) => println!("{token:?}"),
            Err((span, err)) => report_lexer_error(script, span, err),
        }
    }

    Ok(())
}
