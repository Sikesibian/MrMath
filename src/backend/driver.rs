use crate::backend::{
    parser::parse,
    interpreter::interpret
};
use ansi_term::Colour::Red;

pub fn run(input: &str) {
    let tu = parse(input);
    if tu.is_err() {
        println!("{}{}", Red.paint("Error: "), tu.err().unwrap());
        return;
    }
    interpret(tu.unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        run("1234 + 5");
        run(" 1 + (2 * 3) * (5 // 5 + Frac[3, 4])");
    }
}