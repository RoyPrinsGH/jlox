use std::{fmt::Display, iter::Peekable};

use crate::lexer::Token;

pub enum BinOp {
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
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            BinOp::Asterisk => "*",
            BinOp::BangEqual => "!=",
            BinOp::EqualEqual => "==",
            BinOp::Greater => ">",
            BinOp::GreaterEqual => ">=",
            BinOp::Less => "<",
            BinOp::LessEqual => "<=",
            BinOp::Minus => "-",
            BinOp::Plus => "+",
            BinOp::Slash => "/",
        };

        write!(f, "{repr}")
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            Self::Number(num) => &format!("{num}"),
            Self::String(str) => &format!("{str}"),
            Self::True => "true",
            Self::False => "false",
            Self::Nil => "nil",
        };

        write!(f, "{repr}")
    }
}

pub enum UnOp {
    Negate,
    Negative,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negate => write!(f, "!"),
            Self::Negative => write!(f, "-"),
        }
    }
}

pub enum Expr {
    Literal(Literal),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Grouping(Box<Expr>),
    Invalid,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit) => {
                write!(f, "{lit}")
            }
            Expr::Unary(op, expr) => {
                write!(f, "({op} {expr})")
            }
            Expr::Binary(lhs, op, rhs) => {
                write!(f, "({lhs} {op} {rhs})")
            }
            Expr::Grouping(expr) => {
                write!(f, "{expr}")
            }
            Expr::Invalid => {
                write!(f, "?")
            }
        }
    }
}

fn consume_primary<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let Some(nt) = tokens.next() else {
        return Box::new(Expr::Invalid);
    };

    let literal = match nt {
        Token::False => Literal::False,
        Token::True => Literal::True,
        Token::Nil => Literal::Nil,
        Token::Number(n) => Literal::Number(n),
        Token::String(s) => Literal::String(s.to_owned()),
        Token::LeftParenthesis => {
            let expr = consume_expression(tokens);

            let Some(Token::RightParenthesis) = tokens.next() else {
                return Box::new(Expr::Invalid);
            };

            return Box::new(Expr::Grouping(expr));
        }
        _ => return Box::new(Expr::Invalid),
    };

    Box::new(Expr::Literal(literal))
}

fn consume_unary<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let Some(nt) = tokens.peek() else {
        return Box::new(Expr::Invalid);
    };

    let un_op = match nt {
        Token::Bang => UnOp::Negate,
        Token::Minus => UnOp::Negative,
        _ => return consume_primary(tokens),
    };

    _ = tokens.next();
    return Box::new(Expr::Unary(un_op, consume_unary(tokens)));
}

fn consume_factor<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_unary(tokens);

    loop {
        let Some(nt) = tokens.peek() else {
            break;
        };

        let bin_op = match nt {
            Token::Slash => BinOp::Slash,
            Token::Asterisk => BinOp::Asterisk,
            _ => break,
        };

        _ = tokens.next();
        let rhs = consume_unary(tokens);
        ret_expr = Box::new(Expr::Binary(ret_expr, bin_op, rhs))
    }

    ret_expr
}

fn consume_term<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_factor(tokens);

    loop {
        let Some(nt) = tokens.peek() else {
            break;
        };

        let bin_op = match nt {
            Token::Minus => BinOp::Minus,
            Token::Plus => BinOp::Plus,
            _ => break,
        };

        _ = tokens.next();
        let rhs = consume_factor(tokens);
        ret_expr = Box::new(Expr::Binary(ret_expr, bin_op, rhs))
    }

    ret_expr
}

fn consume_comparison<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_term(tokens);

    loop {
        let Some(nt) = tokens.peek() else {
            break;
        };

        let bin_op = match nt {
            Token::Greater => BinOp::Greater,
            Token::GreaterEqual => BinOp::GreaterEqual,
            Token::Less => BinOp::Less,
            Token::LessEqual => BinOp::LessEqual,
            _ => break,
        };

        _ = tokens.next();
        let rhs = consume_term(tokens);
        ret_expr = Box::new(Expr::Binary(ret_expr, bin_op, rhs))
    }

    ret_expr
}

fn consume_equality<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_comparison(tokens);

    loop {
        let Some(nt) = tokens.peek() else {
            break;
        };

        let bin_op = match nt {
            Token::BangEqual => BinOp::BangEqual,
            Token::EqualEqual => BinOp::EqualEqual,
            _ => break,
        };

        _ = tokens.next();
        let rhs = consume_comparison(tokens);
        ret_expr = Box::new(Expr::Binary(ret_expr, bin_op, rhs))
    }

    ret_expr
}

fn consume_expression<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    consume_equality(tokens)
}

pub fn parse<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    consume_expression(tokens)
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{BinOp, Expr, Literal, UnOp, parse},
        lexer::Token,
    };

    #[test]
    fn test_display() {
        let expr = Box::new(Expr::Binary(
            Box::new(Expr::Literal(Literal::True)),
            BinOp::BangEqual,
            Box::new(Expr::Grouping(Box::new(Expr::Unary(UnOp::Negate, Box::new(Expr::Literal(Literal::True)))))),
        ));

        println!("{expr}")
    }

    #[test]
    fn test_ast_parse() {
        let tokens = [Token::True, Token::BangEqual, Token::LeftParenthesis, Token::Bang, Token::Number(123.0), Token::RightParenthesis];

        let expr = parse(&mut tokens.into_iter().peekable());

        println!("{expr}")
    }
}
