use logos::Logos;

mod ast;
mod lex;
mod parse;
mod types;

fn main() {
    loop {
        let line = read_line().unwrap();

        if line.starts_with(":quit") {
            std::process::exit(0);
        } else if line.starts_with(":lex ") {
            let tokens = repl_do_lex(&line[":lex ".len()..]);
            println!("{:?}", tokens);
        } else {
            let tokens = repl_do_lex(&line);
            match tokens {
                Err(span) => {
                    eprintln!("lex error at {} thru {}", span.start, span.end);
                }
                Ok(tokens) => {
                    repl_do_parse(tokens);
                }
            };
        }
    }
}

fn repl_do_parse(tokens: Vec<parse::LexResult>) {
    use winnow::Parser;
    let result = parse::expression.parse(&tokens);
    match result {
        Err(err) => {
            eprintln!("parse error: {:?}", err);
        }
        Ok(a) => {
            println!("{:#?}", a);
        }
    };
}

fn repl_do_lex(line: &str) -> Result<Vec<parse::LexResult<'_>>, std::ops::Range<usize>> {
    let tokens: Result<Vec<parse::LexResult<'_>>, _> = lex::Token::lexer(line)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => Ok(Into::<parse::LexResult>::into((tok, span))),
            Err(()) => Err(span),
        })
        .collect();
    tokens
}

fn read_line() -> std::io::Result<impl std::ops::Deref<Target = str>> {
    use std::io::{stdin, stdout, Write};
    let mut ret = String::new();
    print!("> ");
    stdout().flush()?;
    stdin().read_line(&mut ret)?;
    // trim
    if ret.ends_with('\n') {
        ret.pop();
    }
    if ret.ends_with('\r') {
        ret.pop();
    }
    Ok(ret)
}
