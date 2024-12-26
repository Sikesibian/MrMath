use crate::backend::{
    parser::parse,
    interpreter::{
        interpret,
        Env
    }
};
use ansi_term::Colour::Red;

pub fn run(input: &str, env: &mut Env) {
    let tu = parse(input);
    if tu.is_err() {
        println!("{}{}", Red.paint("Error: "), tu.err().unwrap());
        return;
    }
    interpret(tu.unwrap(), env);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        let mut env = Env::new();
        run("1234 + 5", &mut env);
        run(" 1 + (2 * 3) * (5 // 5 + Frac[3 + 9, 4])", &mut env);
    }
}