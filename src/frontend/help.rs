use std::collections::HashMap;
use ansi_term::Colour::{Yellow, Green, Red, Cyan};
pub fn help_info() {
    println!("{}", Green.paint("Available commands:"));
    println!("  {}: Display this help message.", Green.paint("help"));
    println!("  {}: New variable environment (Clear all the variables).", Green.paint("new"));
    println!("  {}: Display the manual for a specific command.\n    Input `{}` for more details", Green.paint("man <type>"), Cyan.paint("man details"));
    println!("  {}: Display the command history.", Green.paint("history"));
    println!("  {}: Clear the terminal screen.", Green.paint("clear"));
    println!("  {}: Exit the program.", Green.paint("exit"));
}

struct Manual {
    content: String,
}

impl Manual {
    fn new(content: &str) -> Manual {
        Manual {
            content: content.to_string(),
        }
    }

    fn display(&self) { 
        println!("{}", &self.content); 
    }

    fn show_keys() {
        println!("{}", Green.paint("Available Types:"));
        println!("  {}", Cyan.paint(MANUALS.keys().map(|k| k.to_string()).collect::<Vec<_>>().join(", ")))
    }
}

lazy_static::lazy_static! {
    static ref MANUALS: HashMap<&'static str, Manual> = {
        let mut m = HashMap::new();
        m.insert("Variable", Manual::new(include_str!("../files/manual/variable.txt")));
        m.insert("Integer", Manual::new(include_str!("../files/manual/integer.txt")));
        m.insert("Fraction", Manual::new(include_str!("../files/manual/fraction.txt")));
        m.insert("Vector", Manual::new(include_str!("../files/manual/vector.txt")));
        m.insert("Matrix", Manual::new(include_str!("../files/manual/matrix.txt")));
        m
    };
}
pub fn man_info(ty: &str) {
    if ty == "details" {
        Manual::show_keys();
        return;
    }
    MANUALS.get(ty).map_or_else(
        || {
            println!("{}: {}", Red.paint("Error"), Yellow.paint("Manual not found"));
            Manual::show_keys();
        },
        |manual| {
            let mut counter = false;
            let content_cyan = manual.content.split('`').map(|line| {
                if counter {
                    counter = false;
                    format!("{}", Green.paint(line))
                } else {
                    counter = true;
                    format!("{}", line)
                }
            }).collect::<Vec<String>>().join("");
            println!("{}", content_cyan);
        },
    );
}
