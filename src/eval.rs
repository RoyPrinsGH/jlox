use crate::ast::{BinOp, Expr, Literal, UnOp};

fn is_truthy(lit: Literal) -> bool {
    match lit {
        Literal::Number(n) if n == 0.0 => false,
        Literal::False => false,
        Literal::Nil => false,
        _ => true,
    }
}

#[derive(Debug)]
pub enum EvalError {
    CannotEvalInvalidExpr,
    UnOp(UnOp, Literal),
    BinOp(BinOp, Literal, Literal),
}

// Should probably make use of trait impls on Eval, but this works for a first step
pub fn eval(expr: Box<Expr>) -> Result<Literal, EvalError> {
    match *expr {
        Expr::Grouping(inner) => eval(inner),
        Expr::Invalid => Err(EvalError::CannotEvalInvalidExpr),
        Expr::Unary(un_op, inner) => match (un_op, eval(inner)?) {
            (UnOp::Negate, e) => Ok(if is_truthy(e) { Literal::False } else { Literal::True }),
            (UnOp::Negative, Literal::Number(n)) => Ok(Literal::Number(-n)),
            (UnOp::Negative, e) => Err(EvalError::UnOp(UnOp::Negative, e)),
        },
        Expr::Literal(lit) => Ok(lit),
        Expr::Binary(lhs, bin_op, rhs) => match (eval(lhs)?, bin_op, eval(rhs)?) {
            (Literal::Number(a), BinOp::Plus, Literal::Number(b)) => Ok(Literal::Number(a + b)),
            (Literal::String(a), BinOp::Plus, Literal::String(b)) => {
                let mut ac = a.clone();
                ac.push_str(&b);
                Ok(Literal::String(ac))
            }
            (Literal::Number(a), BinOp::Minus, Literal::Number(b)) => Ok(Literal::Number(a - b)),
            (a, BinOp::EqualEqual, b) => Ok(if a == b { Literal::True } else { Literal::False }),
            (a, BinOp::BangEqual, b) => Ok(if a != b { Literal::True } else { Literal::False }),
            (Literal::Number(a), BinOp::Asterisk, Literal::Number(b)) => Ok(Literal::Number(a * b)),
            (Literal::Number(a), BinOp::Slash, Literal::Number(b)) => Ok(Literal::Number(a / b)),
            (Literal::Number(a), BinOp::Less, Literal::Number(b)) => Ok(if a < b { Literal::False } else { Literal::True }),
            (Literal::Number(a), BinOp::LessEqual, Literal::Number(b)) => Ok(if a <= b { Literal::False } else { Literal::True }),
            (Literal::Number(a), BinOp::Greater, Literal::Number(b)) => Ok(if a > b { Literal::False } else { Literal::True }),
            (Literal::Number(a), BinOp::GreaterEqual, Literal::Number(b)) => Ok(if a >= b { Literal::False } else { Literal::True }),
            (l, b, r) => Err(EvalError::BinOp(b, l, r)),
        },
    }
}
