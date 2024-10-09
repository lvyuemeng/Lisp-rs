use crate::env::Env;
use crate::object::EvalResult;
use crate::parser::parse;

pub fn eval(program: &str, env: &Env) -> EvalResult {
    parse(program)
        .map_err(|e| e.to_string())
        .and_then(|parsed| parsed.eval(env))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{env::new_env, object::Object};

    #[test]
    fn test_simple_define() {
        let env = new_env();
        let result = eval("((define r 10) (define pi 314) (define pix 323))", &env).unwrap();
        assert_eq!(result, Object::Void);
    }

    #[test]
    fn test_simple_add() {
        let env = new_env();
        let result = eval("((define r 10) (+ 1 2))", &env).unwrap();
        assert_eq!(result, Object::Integer(3));
    }

    #[test]
    fn test_area_of_a_circle() {
        let env = new_env();
        let program = "(
                        (define r 10)
                        (define pi 314)
                        (* pi (* r r))
                      )";
        let result = eval(program, &env);
        assert_eq!(result, Ok(Object::Integer(314 * 10 * 10)));
    }

    #[test]
    fn test_sqr_function() {
        let env = new_env();
        let program = "(
                        (define sqr (lambda (r) (* r r))) 
                        (sqr 10)
                       )";
        let result = eval(program, &env).unwrap();
        assert_eq!(result, Object::Integer(10 * 10));
    }

    #[test]
    fn test_fibonacci() {
        let env = new_env();
        let program = "
            (
                (define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
                (fib 10)
            )
        ";

        let result = eval(program, &env).unwrap();
        assert_eq!(result, Object::Integer(89));
    }

    #[test]
    fn test_factorial() {
        let env = new_env();
        let program = "
            (
                (define fact (lambda (n) (if (< n 1) 1 (* n (fact (- n 1))))))
                (fact 5)
            )
        ";

        let result = eval(program, &env).unwrap();
        assert_eq!(result, Object::Integer(120));
    }

    #[test]
    fn test_circle_area_function() {
        let env = new_env();
        let program = "
            (
                (define pi 314)
                (define r 10)
                (define sqr (lambda (r) (* r r)))
                (define area (lambda (r) (* pi (sqr r))))
                (area r)
            )
        ";

        let result = eval(program, &env).unwrap();
        assert_eq!(result, Object::Integer(314 * 10 * 10));
    }
}
