use crate::env::*;
use crate::eval::eval;
use crate::lexer::Token;
use std::fmt;

pub type EvalResult = Result<Object, String>;

trait Eval {
    fn eval(&self, args: Vec<Object>, env: &Env) -> EvalResult;
}

impl Eval for Keyword {
    fn eval(&self, args: Vec<Object>, env: &Env) -> EvalResult {
        match self {
            Keyword::Define => self.execute_define(args, env),
            Keyword::If => self.execute_if(args, env),
            Keyword::Lambda => self.execute_lambda(args),
        }
    }
}

impl Eval for Operator {
    fn eval(&self, args: Vec<Object>, env: &Env) -> EvalResult {
        args.into_iter()
            .map(|arg| arg.eval(env))
            .collect::<Result<Vec<_>, _>>()
            .and_then(|evaluated_args| {
                if let [left, right] = &evaluated_args[..] {
                    self.apply(left, right)
                } else {
                    Err("Operator requires exactly two arguments".to_string())
                }
            })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Void,
    Integer(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Symbol(String),
    Lambda(Vec<String>, Vec<Object>),
    List(Vec<Object>),
    Operator(Operator),
    Keyword(Keyword),
}

impl Object {
    pub fn from_token(token: &Token) -> Result<Self, String> {
        match token {
            Token::Atom(s) => s
                .parse::<i64>()
                .map(Object::Integer)
                .or_else(|_| s.parse::<f64>().map(Object::Float))
                .or_else(|_| match s.as_str() {
                    "true" => Ok(Object::Bool(true)),
                    "false" => Ok(Object::Bool(false)),
                    _ => Ok(Operator::from_str(s)
                        .map(Object::Operator)
                        .or_else(|| Keyword::from_str(s).map(Object::Keyword))
                        .unwrap_or_else(|| Object::Symbol(s.to_string()))),
                }),
            Token::LParen | Token::RParen => Err("Parentheses are not valid Objects".to_string()),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Bool(false) | Object::Void)
    }

    pub fn eval(&self, env: &Env) -> EvalResult {
        match self {
            Object::Symbol(s) => env
                .borrow()
                .get(s)
                .ok_or_else(|| format!("Unbound symbol: {}", s)),
            Object::List(list) => eval_list(list.clone(), env),
            _ => Ok(self.clone()),
        }
    }
}

fn eval_list(list: Vec<Object>, env: &Env) -> EvalResult {
    list.split_first()
        .ok_or_else(|| "Empty list".to_string())
        .and_then(|(first, rest)| match first {
            Object::Keyword(kw) => kw.eval(rest.to_vec(), env),
            Object::Operator(op) => op.eval(rest.to_vec(), env),
            _ => evaluate_and_apply(list.clone(), env),
        })
}

fn evaluate_and_apply(list: Vec<Object>, env: &Env) -> EvalResult {
    list.iter()
        .map(|obj| obj.eval(env))
        .filter(|res| !matches!(res, Ok(Object::Void)))
        .collect::<Result<Vec<_>, _>>()
        .and_then(|evaluated| match evaluated.as_slice() {
            [] => Ok(Object::Void),
            [single] => Ok(single.clone()),
            [func, args @ ..] => apply_function(func, args.to_vec(), env),
        })
}

fn apply_function(func: &Object, args: Vec<Object>, env: &Env) -> EvalResult {
    match func {
        Object::Lambda(params, body) => apply_lambda(params, body, args, env),
        Object::Symbol(name) => env
            .borrow()
            .get(name)
            .ok_or_else(|| format!("Unbound symbol: {}", name))
            .and_then(|obj| apply_function(&obj, args, env)),
        _ => Err("First item of function call must evaluate to a lambda".to_string()),
    }
}

fn apply_lambda(params: &[String], body: &[Object], args: Vec<Object>, env: &Env) -> EvalResult {
    match params.len().cmp(&args.len()) {
        std::cmp::Ordering::Equal => {
            let new_env = extend_env(env.clone());
            params.iter().zip(args).for_each(|(param, arg)| {
                new_env.borrow_mut().set(param, arg);
            });
            body.iter()
                .try_fold(Object::Void, |_, expr| expr.eval(&new_env))
        }
        std::cmp::Ordering::Greater => {
            let new_params = params[args.len()..].to_vec();
            let new_env = extend_env(env.clone());
            params.iter().zip(args).for_each(|(param, arg)| {
                new_env.borrow_mut().set(param, arg);
            });
            Ok(Object::Lambda(new_params, body.to_vec()))
        }
        std::cmp::Ordering::Less => {
            let (current_args, remaining_args) = args.split_at(params.len());
            let new_env = extend_env(env.clone());
            params.iter().zip(current_args).for_each(|(param, arg)| {
                new_env.borrow_mut().set(param, arg.clone());
            });
            body.iter()
                .try_fold(Object::Void, |_, expr| expr.eval(&new_env))
                .and_then(|result| apply_function(&result, remaining_args.to_vec(), env))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
}

impl Operator {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Operator::Add),
            "-" => Some(Operator::Subtract),
            "*" => Some(Operator::Multiply),
            "/" => Some(Operator::Divide),
            "<" => Some(Operator::LessThan),
            ">" => Some(Operator::GreaterThan),
            "=" => Some(Operator::Equal),
            "!=" => Some(Operator::NotEqual),
            _ => None,
        }
    }

    fn apply(&self, left: &Object, right: &Object) -> EvalResult {
        use Operator::*;
        match (self, left, right) {
            (Add, Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l + r)),
            (Add, Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
            (Subtract, Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l - r)),
            (Subtract, Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
            (Multiply, Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l * r)),
            (Multiply, Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
            (Divide, Object::Integer(l), Object::Integer(r)) => Ok(Object::Integer(l / r)),
            (Divide, Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
            (LessThan, Object::Integer(l), Object::Integer(r)) => Ok(Object::Bool(l < r)),
            (LessThan, Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l < r)),
            (GreaterThan, Object::Integer(l), Object::Integer(r)) => Ok(Object::Bool(l > r)),
            (GreaterThan, Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l > r)),
            (Equal, Object::Integer(l), Object::Integer(r)) => Ok(Object::Bool(l == r)),
            (Equal, Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l == r)),
            (NotEqual, Object::Integer(l), Object::Integer(r)) => Ok(Object::Bool(l != r)),
            (NotEqual, Object::Float(l), Object::Float(r)) => Ok(Object::Bool(l != r)),
            _ => Err(format!(
                "Operation not supported for these types: {:?} {:?} {:?}",
                self, left, right
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Define,
    If,
    Lambda,
}

impl Keyword {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "define" => Some(Keyword::Define),
            "if" => Some(Keyword::If),
            "lambda" => Some(Keyword::Lambda),
            _ => None,
        }
    }

    fn execute_define(&self, mut args: Vec<Object>, env: &Env) -> EvalResult {
        if args.len() < 2 {
            return Err("Define requires a symbol and a value".to_string());
        }
        let symbol = args.remove(0);
        let value = args.remove(0);
        match symbol {
            Object::Symbol(s) => {
                let evaluated_value = value.eval(env)?;
                env.borrow_mut().set(&s, evaluated_value);
                Ok(Object::Void)
            }
            _ => Err("First argument of define must be a symbol".to_string()),
        }
    }

    fn execute_if(&self, mut args: Vec<Object>, env: &Env) -> EvalResult {
        if args.len() < 2 {
            return Err("If requires at least two arguments".to_string());
        }
        let cond = args.remove(0);
        let then = args.remove(0);
        let else_ = args.pop();

        if cond.eval(env)?.is_truthy() {
            then.eval(env)
        } else if let Some(else_expr) = else_ {
            else_expr.eval(env)
        } else {
            Ok(Object::Void)
        }
    }

    fn execute_lambda(&self, args: Vec<Object>) -> EvalResult {
        if args.len() < 2 {
            return Err("Lambda requires parameters and a body".to_string());
        }
        let params = args[0].clone();
        let body = args[1..].to_vec();

        match params {
            Object::List(params_list) => {
                let params = params_list
                    .into_iter()
                    .map(|param| {
                        if let Object::Symbol(s) = param {
                            Ok(s)
                        } else {
                            Err("Lambda parameters must be symbols".to_string())
                        }
                    })
                    .collect::<Result<Vec<String>, String>>()?;
                Ok(Object::Lambda(params, body))
            }
            _ => Err("Invalid lambda syntax".to_string()),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Void => write!(f, "Void"),
            Object::Integer(n) => write!(f, "{}", n),
            Object::Float(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Bool(b) => write!(f, "{}", b),
            Object::Symbol(s) => write!(f, "{}", s),
            Object::Lambda(params, body) => {
                write!(
                    f,
                    "(lambda ({}) {})",
                    params.join(" "),
                    body.iter()
                        .map(|expr| expr.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Object::List(list) => {
                write!(
                    f,
                    "({})",
                    list.iter()
                        .map(|obj| obj.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Object::Operator(op) => write!(f, "{:?}", op),
            Object::Keyword(kw) => write!(f, "{:?}", kw),
        }
    }
}
