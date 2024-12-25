use ansi_term::Colour::Green;
pub fn help_info() {
    println!("{}", Green.paint("Available commands:"));
    println!("  {}: Display this help message.", Green.paint("help"));
    println!("  {}: Display the command history.", Green.paint("history"));
    println!("  {}: Clear the terminal screen.", Green.paint("clear"));
    println!("  {}: Exit the program.", Green.paint("exit"));
}