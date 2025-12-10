use std::{fmt::Display, iter::Peekable, ops::Range};

use crate::lexer::{Token, TokenData};

#[derive(Debug)]
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

#[derive(Debug)]
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

pub struct Expr {
    pub data: ExprData,
    pub span: Range<usize>,
}

impl Expr {
    fn new_boxed(data: ExprData, span: Range<usize>) -> Box<Self> {
        Box::new(Self { data, span })
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} @[{} - {}]", self.data, self.span.start, self.span.end - 1)
    }
}

pub enum ExprData {
    Literal(Literal),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Grouping(Box<Expr>),
    Invalid,
}

impl Display for ExprData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprData::Literal(lit) => {
                write!(f, "{lit}")
            }
            ExprData::Unary(op, expr) => {
                write!(f, "({op} {expr})")
            }
            ExprData::Binary(lhs, op, rhs) => {
                write!(f, "({lhs} {op} {rhs})")
            }
            ExprData::Grouping(expr) => {
                write!(f, "{expr}")
            }
            ExprData::Invalid => {
                write!(f, "?")
            }
        }
    }
}

fn consume_primary<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let Some(token) = tokens.next() else {
        return Expr::new_boxed(ExprData::Invalid, 0..0);
    };
    let literal = match token.data {
        TokenData::False => Literal::False,
        TokenData::True => Literal::True,
        TokenData::Nil => Literal::Nil,
        TokenData::Number(n) => Literal::Number(n),
        TokenData::String(s) => Literal::String(s.to_owned()),
        TokenData::LeftParenthesis => {
            let expr = consume_expression(tokens);
            let Some(Token {
                span,
                data: TokenData::RightParenthesis,
            }) = tokens.next()
            else {
                return Expr::new_boxed(ExprData::Invalid, token.span.start..expr.span.end);
            };
            return Expr::new_boxed(ExprData::Grouping(expr), token.span.start..span.end);
        }
        _ => {
            return Expr::new_boxed(ExprData::Invalid, token.span);
        }
    };
    return Expr::new_boxed(ExprData::Literal(literal), token.span);
}

fn consume_unary<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let Some(next_token) = tokens.peek() else {
        return Expr::new_boxed(ExprData::Invalid, 0..0);
    };
    let un_op = match next_token.data {
        TokenData::Bang => UnOp::Negate,
        TokenData::Minus => UnOp::Negative,
        _ => return consume_primary(tokens),
    };
    let un_op_span_start = next_token.span.start;
    _ = tokens.next();
    let unary_expr = consume_unary(tokens);
    let unary_expr_span_end = unary_expr.span.end;
    return Expr::new_boxed(ExprData::Unary(un_op, unary_expr), un_op_span_start..unary_expr_span_end);
}

fn consume_factor<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_unary(tokens);
    loop {
        let Some(next_token) = tokens.peek() else {
            break;
        };
        let bin_op = match next_token.data {
            TokenData::Slash => BinOp::Slash,
            TokenData::Asterisk => BinOp::Asterisk,
            _ => break,
        };
        let ret_expr_span_start = ret_expr.span.start;
        _ = tokens.next();
        let rhs = consume_factor(tokens);
        let rhs_span_end = rhs.span.end;
        ret_expr = Expr::new_boxed(ExprData::Binary(ret_expr, bin_op, rhs), ret_expr_span_start..rhs_span_end);
    }
    ret_expr
}

fn consume_term<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_factor(tokens);
    loop {
        let Some(next_token) = tokens.peek() else {
            break;
        };

        let bin_op = match next_token.data {
            TokenData::Minus => BinOp::Minus,
            TokenData::Plus => BinOp::Plus,
            _ => break,
        };
        let ret_expr_span_start = ret_expr.span.start;
        _ = tokens.next();
        let rhs = consume_factor(tokens);
        let rhs_span_end = rhs.span.end;
        ret_expr = Expr::new_boxed(ExprData::Binary(ret_expr, bin_op, rhs), ret_expr_span_start..rhs_span_end);
    }
    ret_expr
}

fn consume_comparison<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_term(tokens);
    loop {
        let Some(next_token) = tokens.peek() else {
            break;
        };
        let bin_op = match next_token.data {
            TokenData::Greater => BinOp::Greater,
            TokenData::GreaterEqual => BinOp::GreaterEqual,
            TokenData::Less => BinOp::Less,
            TokenData::LessEqual => BinOp::LessEqual,
            _ => break,
        };
        let ret_expr_span_start = ret_expr.span.start;
        _ = tokens.next();
        let rhs = consume_term(tokens);
        let rhs_span_end = rhs.span.end;
        ret_expr = Expr::new_boxed(ExprData::Binary(ret_expr, bin_op, rhs), ret_expr_span_start..rhs_span_end);
    }
    ret_expr
}

fn consume_equality<'t, Tokens>(tokens: &mut Peekable<Tokens>) -> Box<Expr>
where
    Tokens: Iterator<Item = Token<'t>>,
{
    let mut ret_expr = consume_comparison(tokens);
    loop {
        let Some(next_token) = tokens.peek() else {
            break;
        };
        let bin_op = match next_token.data {
            TokenData::BangEqual => BinOp::BangEqual,
            TokenData::EqualEqual => BinOp::EqualEqual,
            _ => break,
        };
        let ret_expr_span_start = ret_expr.span.start;
        _ = tokens.next();
        let rhs = consume_comparison(tokens);
        let rhs_span_end = rhs.span.end;
        ret_expr = Expr::new_boxed(ExprData::Binary(ret_expr, bin_op, rhs), ret_expr_span_start..rhs_span_end);
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
