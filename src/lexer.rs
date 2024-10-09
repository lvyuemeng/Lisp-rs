use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Atom(String),
    LParen,
    RParen,
}

#[derive(Debug)]
pub struct TokenError {
    message: String,
}

impl Error for TokenError {}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Token error: {}", self.message)
    }
}

trait Tokenizable {
    fn tokenize(&self) -> Result<Token, TokenError>;
}

impl Tokenizable for &str {
    fn tokenize(&self) -> Result<Token, TokenError> {
        match *self {
            "(" => Ok(Token::LParen),
            ")" => Ok(Token::RParen),
            _ => Ok(Token::Atom(self.to_string())),
        }
    }
}

pub fn tokenize(program: &str) -> Result<Vec<Token>, TokenError> {
    program
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|word| word.tokenize())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let tokens = tokenize("(+ 1 2)").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Atom("+".to_string()),
                Token::Atom("1".to_string()),
                Token::Atom("2".to_string()),
                Token::RParen,
            ]
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
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::LParen,
                Token::Atom("define".to_string()),
                Token::Atom("r".to_string()),
                Token::Atom("10".to_string()),
                Token::RParen,
                Token::LParen,
                Token::Atom("define".to_string()),
                Token::Atom("pi".to_string()),
                Token::Atom("314".to_string()),
                Token::RParen,
                Token::LParen,
                Token::Atom("*".to_string()),
                Token::Atom("pi".to_string()),
                Token::LParen,
                Token::Atom("*".to_string()),
                Token::Atom("r".to_string()),
                Token::Atom("r".to_string()),
                Token::RParen,
                Token::RParen,
                Token::RParen
            ]
        );
    }
}
