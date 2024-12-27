use std::io::{self, Write};
use ansi_term::Colour::{Yellow, Green};
use crate::frontend::help::man_info;
use crate::frontend::{help, history};
use crate::backend::{
    driver::run,
    interpreter::interpreter::Env
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
        let mut parts = input.trim().split_whitespace();
        match input.to_lowercase().as_str() {
            "" => continue,
            "exit" | "quit" => break,
            "help" => help::help_info(),
            "history" => history::history_info(),
            "clear" => println!("{esc}[2J{esc}[1;1HInput {msg} to get detailed info.\n", esc = 27 as char, msg = Green.paint("help")),
            "new" => {
                env.clear();
                println!("{}", Green.paint("New environment created!"));
            },
            _ => {
                // println!("{}", Red.paint("Invalid command!"))
                if let Some(command) = parts.next() {
                    match command {
                        "man" => {
                            if let Some(ty) = parts.next() { man_info(ty); } 
                            else { println!("Usage: man <type>"); }
                        },
                        _ => run(&input, &mut env),
                    }
                }
                // run(&input, &mut env);
            },
        }
    }
}