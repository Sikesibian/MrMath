use ansi_term::Colour::Green;

static mut HISTORY: Vec<String> = Vec::new();
static mut CURRENT_INDEX: usize = 0;

pub fn add_to_history(command: String) {
    unsafe {
        HISTORY.push(command);
        CURRENT_INDEX = HISTORY.len() - 1;
    }
}

pub fn history_info() {
    println!("{}", Green.paint("History:"));
    for (i, command) in unsafe { HISTORY.iter().enumerate() } {
        println!("  {}: {}", i + 1, command);
    }
}