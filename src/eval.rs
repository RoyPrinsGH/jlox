use crate::ast::{BinOp, Expr, Literal, UnOp};

#[derive(Debug, PartialEq)]
pub enum Eval {
    Invalid,
    Value(Literal),
}

fn is_truthy(eval: Eval) -> bool {
    match eval {
        Eval::Value(Literal::Number(n)) if n == 0.0 => false,
        Eval::Value(Literal::False) => false,
        Eval::Value(Literal::Nil) => false,
        _ => true,
    }
}

// Should probably make use of trait impls on Eval, but this works for a first step
pub fn eval(expr: Box<Expr>) -> Eval {
    match *expr {
        Expr::Grouping(inner) => eval(inner),
        Expr::Invalid => Eval::Invalid,
        Expr::Unary(un_op, inner) => match (un_op, eval(inner)) {
            (UnOp::Negate, e) => Eval::Value(if is_truthy(e) { Literal::False } else { Literal::True }),
            (UnOp::Negative, Eval::Value(Literal::Number(n))) => Eval::Value(Literal::Number(-n)),
            (UnOp::Negative, _) => Eval::Invalid,
        },
        Expr::Literal(lit) => Eval::Value(lit),
        Expr::Binary(lhs, bin_op, rhs) => match (eval(lhs), bin_op, eval(rhs)) {
            (Eval::Value(Literal::Number(a)), BinOp::Plus, Eval::Value(Literal::Number(b))) => Eval::Value(Literal::Number(a + b)),
            (Eval::Value(Literal::String(a)), BinOp::Plus, Eval::Value(Literal::String(b))) => {
                let mut ac = a.clone();
                ac.push_str(&b);
                Eval::Value(Literal::String(ac))
            }
            (_, BinOp::Plus, _) => Eval::Invalid,
            (Eval::Value(Literal::Number(a)), BinOp::Minus, Eval::Value(Literal::Number(b))) => Eval::Value(Literal::Number(a - b)),
            (_, BinOp::Minus, _) => Eval::Invalid,
            (a, BinOp::EqualEqual, b) => {
                if a == b {
                    Eval::Value(Literal::True)
                } else {
                    Eval::Value(Literal::False)
                }
            }
            (a, BinOp::BangEqual, b) => {
                if a != b {
                    Eval::Value(Literal::True)
                } else {
                    Eval::Value(Literal::False)
                }
            }
            (Eval::Value(Literal::Number(a)), BinOp::Asterisk, Eval::Value(Literal::Number(b))) => Eval::Value(Literal::Number(a * b)),
            (_, BinOp::Asterisk, _) => Eval::Invalid,
            (Eval::Value(Literal::Number(a)), BinOp::Slash, Eval::Value(Literal::Number(b))) => Eval::Value(Literal::Number(a / b)),
            (_, BinOp::Slash, _) => Eval::Invalid,
            (Eval::Value(Literal::Number(a)), BinOp::Less, Eval::Value(Literal::Number(b))) => Eval::Value(if a < b { Literal::False } else { Literal::True }),
            (_, BinOp::Less, _) => Eval::Invalid,
            (Eval::Value(Literal::Number(a)), BinOp::LessEqual, Eval::Value(Literal::Number(b))) => Eval::Value(if a <= b { Literal::False } else { Literal::True }),
            (_, BinOp::LessEqual, _) => Eval::Invalid,
            (Eval::Value(Literal::Number(a)), BinOp::Greater, Eval::Value(Literal::Number(b))) => Eval::Value(if a > b { Literal::False } else { Literal::True }),
            (_, BinOp::Greater, _) => Eval::Invalid,
            (Eval::Value(Literal::Number(a)), BinOp::GreaterEqual, Eval::Value(Literal::Number(b))) => Eval::Value(if a >= b { Literal::False } else { Literal::True }),
            (_, BinOp::GreaterEqual, _) => Eval::Invalid,
        },
    }
}
