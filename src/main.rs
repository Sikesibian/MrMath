mod hello;
mod frontend;
mod backend;

fn main() {
    hello::intro_info();
    frontend::service::console();
    hello::bye_info();
}