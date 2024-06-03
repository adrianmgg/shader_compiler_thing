use logos::Logos;

mod lex;
mod parse;
mod ast;

fn main() {
    let input = "hello world 123";
    let lex = lex::Token::lexer(input);
    let tokens: Vec<_> = lex.collect();
    dbg!(tokens);
}
