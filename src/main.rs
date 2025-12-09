use std::{
    fs::read_to_string,
    io::{Write, stdin, stdout},
    ops::Range,
    path::{Path, PathBuf},
};

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use miette::{Context, IntoDiagnostic, Result, miette};

use crate::{
    ast::parse,
    eval::eval,
    lexer::{LexerError, lex},
};

pub mod ast;
pub mod eval;
pub mod lexer;

#[derive(Parser, Debug)]
struct Args {
    /// Name of the JLox script to run
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
    let script_content = read_to_string(script_path).into_diagnostic().wrap_err(miette!("Specified script {:?} not found", script_path))?;
    run(script_content)
}

fn run_prompt() -> Result<()> {
    let mut line_to_execute = String::new();

    loop {
        print!("> ");

        stdout().flush().into_diagnostic().wrap_err(miette!("Failed to flush stdout: {:?}", line_to_execute))?;

        stdin()
            .read_line(&mut line_to_execute)
            .into_diagnostic()
            .wrap_err(miette!("Failed to read from stdin: {:?}", line_to_execute))?;

        run(&line_to_execute)?;

        line_to_execute.clear();
    }
}

fn report_lexer_error(script_name: &str, script_source: &str, span: Range<usize>, error: LexerError) {
    let mut report_builder = Report::build(ReportKind::Error, (script_name, span.start..span.end));

    match error {
        LexerError::UnfinishedString => {
            report_builder = report_builder.with_code(1).with_message("Unfinished string").with_label(
                Label::new((script_name, span.end..span.end))
                    .with_message("Expected end of string here: Is your source well-formatted?")
                    .with_color(Color::Red),
            )
        }
        LexerError::UnrecognisedToken => {
            report_builder = report_builder.with_code(2).with_message("Unrecognised token").with_label(
                Label::new((script_name, span.start..span.end))
                    .with_message("This is not a valid JLox token")
                    .with_color(Color::Red),
            )
        }
        LexerError::InvalidNumber => {
            report_builder = report_builder.with_code(3).with_message("Invalid number").with_label(
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

    let mut tokens = lex(script)
        .filter_map(|res| match res {
            Ok(token) => {
                println!("{token:?}");
                Some(token)
            }
            Err((span, err)) => {
                report_lexer_error("repl", script, span, err);
                None
            }
        })
        .peekable();

    let expr = parse(&mut tokens);

    println!("-- Parsed expr = {expr}");
    println!("-- Remaining tokens: ");

    _ = tokens.last();

    let eval = eval(expr);

    println!("-- Evaluated: {eval:?}");

    Ok(())
}
