use crate::lexer::*;
use crate::object::*;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct ParseError {
    err: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}", self.err)
    }
}

impl Error for ParseError {}

pub fn parse(program: &str) -> Result<Object, ParseError> {
    let tokens = tokenize(program).map_err(|err| ParseError {
        err: format!("{}", err),
    })?;

    let mut iter = tokens.iter();
    parse_expression(&mut iter)
}

fn parse_expression<'a, I>(tokens: &mut I) -> Result<Object, ParseError>
where
    I: Iterator<Item = &'a Token>,
{
    match tokens.next() {
        Some(Token::LParen) => parse_iter(tokens),
        Some(Token::RParen) => Err(ParseError {
            err: "Unexpected closing parenthesis".to_string(),
        }),
        Some(token) => Object::from_token(token).map_err(|_| ParseError {
            err: format!("Unexpected token: {:?}", token),
        }),
        None => Err(ParseError {
            err: "Unexpected end of input".to_string(),
        }),
    }
}

fn parse_iter<'a, I>(tokens: &mut I) -> Result<Object, ParseError>
where
    I: Iterator<Item = &'a Token>,
{
    let mut elements = Vec::new();

    loop {
        match tokens.next() {
            Some(Token::RParen) => return Ok(Object::List(elements)),
            Some(token) => {
                let element = match token {
                    Token::LParen => parse_iter(tokens)?,
                    _ => Object::from_token(token).map_err(|_| ParseError {
                        err: format!("Unexpected token: {:?}", token),
                    })?,
                };
                elements.push(element);
            }
            None => {
                return Err(ParseError {
                    err: "Unmatched opening parenthesis".to_string(),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;
    use crate::object::{Object, Operator, Keyword};

    #[test]
    fn test_add() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        let list = parse_expression(&mut tokens.iter()).unwrap();
        assert_eq!(
            list,
            Object::List(vec![
                Object::Operator(Operator::Add),
                Object::Integer(1),
                Object::Integer(2),
            ])
        );
    }

    #[test]
    fn test_area_of_a_circle() {
        let program = "
            (
                (define r 10)
                (define pi 314)
                (* pi (* r r))
            )
        ";
        let tokens = tokenize(program).unwrap();
        let list = parse_expression(&mut tokens.iter()).unwrap();
        assert_eq!(
            list,
            Object::List(vec![
                Object::List(vec![
                    Object::Keyword(Keyword::Define),
                    Object::Symbol("r".to_string()),
                    Object::Integer(10),
                ]),
                Object::List(vec![
                    Object::Keyword(Keyword::Define),
                    Object::Symbol("pi".to_string()),
                    Object::Integer(314),
                ]),
                Object::List(vec![
                    Object::Operator(Operator::Multiply),
                    Object::Symbol("pi".to_string()),
                    Object::List(vec![
                        Object::Operator(Operator::Multiply),
                        Object::Symbol("r".to_string()),
                        Object::Symbol("r".to_string()),
                    ]),
                ]),
            ])
        );
    }

    #[test]
    fn test_if_expression() {
        let program = "(if (< 1 2) 3 4)";
        let tokens = tokenize(program).unwrap();
        let list = parse_expression(&mut tokens.iter()).unwrap();
        assert_eq!(
            list,
            Object::List(vec![
                Object::Keyword(Keyword::If),
                Object::List(vec![
                    Object::Operator(Operator::LessThan),
                    Object::Integer(1),
                    Object::Integer(2),
                ]),
                Object::Integer(3),
                Object::Integer(4),
            ])
        );
    }
}
