use ansi_term::Colour::{Yellow, Green};

const VERSION: &str = "v0.1.2";

pub fn intro_info() {
    // print the ascii art
    println!(r"{}[2J{}[1;1H
┌────────────────────────────────────────────────────────────────────┐
│        _  _     _ _     _   __  __     __  __      _   _           │
│       | || |___| | |___| | |  \/  |_ _|  \/  |__ _| |_| |_         │
│       | __ / -_) | / _ \_| | |\/| | '_| |\/| / _` |  _| ' \        │
│       |_||_\___|_|_\___(_) |_|  |_|_| |_|  |_\__,_|\__|_||_|       │
│                                                                    │
│         Welcome to {} Calculator!        │
└────────────────────────────────────────────────────────────────────┘
Input `{}` to get detailed info.
", 27 as char, 27 as char, Yellow.paint(format!("My Rust Math (MrMath {})", VERSION)), Green.paint("help"));
}

pub fn bye_info() {
    println!("\n[ Thank you for using {}! ]\n{}", Yellow.paint(format!("MrMath {}", VERSION)), Green.paint("Have a nice day!"));
}