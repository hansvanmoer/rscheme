mod lexer;
mod pos;

use lexer::Lexer;

fn main() {
    println!("Hello, world!");
    let mut lexer = Lexer::new("test test");
    lexer.lex().unwrap().unwrap();
    lexer.line();
    lexer.col();
}
