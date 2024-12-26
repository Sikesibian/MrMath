use std::io::{self, Write};
use ansi_term::Colour::{Yellow, Green};
use crate::frontend::{help, history};
use crate::backend::{
    driver::run,
    interpreter::Env
};

pub fn console() {        
    let mut env = Env::new();
    loop {
        print!("{}: ", Yellow.paint("MrMath"));
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Error: Failed to read line!");
        input = input.trim().to_string();
        history::add_to_history(input.clone());
        // let mut parts = input.trim().split_whitespace();
        match input.to_lowercase().as_str() {
            "" => continue,
            "exit" | "quit" => break,
            "help" => help::help_info(),
            "history" => history::history_info(),
            "clear" => print!("{esc}[2J{esc}[1;1H", esc = 27 as char),
            "new" => {
                env.clear();
                println!("{}", Green.paint("New environment created!"));
            },
            _ => {
                // println!("{}", Red.paint("Invalid command!"))
                run(&input, &mut env);
            },
        }
    }
}