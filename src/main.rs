#![allow(unused)]

use logos::Logos;

mod ast;
mod lex;
mod parse;
mod types;

fn main() {
    let args: Vec<_> = std::env::args().skip(1).collect();
    if !args.is_empty() {
        for arg in args {
            handle_line(&arg);
        }
    } else {
        loop {
            let line = read_line().unwrap();
            handle_line(&line);
        }
    }
}

fn handle_line(line: &str) {
    if line.starts_with(":quit") {
        std::process::exit(0);
    } else if let Some(line) = line.strip_prefix(":lex ") {
        println!("{:?}", repl_do_lex(line));
    } else if let Some(line) = line.strip_prefix(":parse ") {
        repl_do_lex(line).and_then(repl_do_parse).inspect(|a| {
            println!("{:?}", a);
        });
    } else {
        if let Some(n) = repl_do_lex(line).and_then(repl_do_parse) {
            repl_do_typecheck(n);
        }
    }
}

fn repl_do_typecheck(root: ast::Function<'_, ()>) {
    use ast::{ASTVisitor, DirectlyTypedNode, Node, VisitableNode};

    let mut engine = types::Engine::new();
    let mut root = root
        .upgrade(&|_type| -> Result<Option<types::TypeId>, ()> { Ok(None) })
        .unwrap();

    struct InitialTypesVisitor<'e> {
        engine: &'e mut types::Engine,
    };
    impl<'s, 'e> ASTVisitor<'s, Option<types::TypeId>> for InitialTypesVisitor<'e> {
        type Error = ();
        fn visit_expression(
            &mut self,
            node: &mut ast::Expression<'s, Option<types::TypeId>>,
        ) -> Result<(), Self::Error> {
            match node {
                ast::Expression::VariableReference(v) => {
                    v.r#type = Some(self.engine.insert(types::TypeInfo::Unknown));
                }
                ast::Expression::Number(n) => {
                    n.r#type = Some(self.engine.insert(types::TypeInfo::Integer(types::Integer {
                        width: n.val.bit_width(),
                        signed: n.val.is_signed(),
                    })));
                }
                ast::Expression::BinaryInfix(ex) => {
                    ex.r#type = Some(self.engine.insert(types::TypeInfo::Unknown));
                }
                ast::Expression::FunctionCall(_) => {}
            };
            Ok(())
        }
        fn visit_typename_node(
            &mut self,
            node: &mut ast::TypeName<'s, Option<types::TypeId>>,
        ) -> Result<(), Self::Error> {
            match node {
                ast::TypeName::Named { name, r#type } => {
                    *r#type = Some(self.engine.insert(match name.name.as_slice() {
                        "u32" => types::TypeInfo::Integer(types::Integer {
                            width: 32,
                            signed: false,
                        }),
                        _ => unimplemented!(),
                    }));
                }
            };
            Ok(())
        }
    }
    root.visit(&mut InitialTypesVisitor {
        engine: &mut engine,
    });

    // add typeids to functions in a second pass, since for that we need to gurantee the functions
    // ret/args already got theirs and i'd rather this not be implicitly dependent on the visiting
    // order
    struct Phase2TypesVisitor<'e> {
        engine: &'e mut types::Engine,
    };
    impl<'s, 'e> ASTVisitor<'s, Option<types::TypeId>> for Phase2TypesVisitor<'e> {
        type Error = ();
        fn visit_function(
            &mut self,
            node: &mut ast::Function<'s, Option<types::TypeId>>,
        ) -> Result<(), Self::Error> {
            node.r#type = Some(
                self.engine
                    .insert(types::TypeInfo::Function(types::Function {
                        return_type: node.return_type.r#type().unwrap(),
                        parameter_types: node
                            .args
                            .iter()
                            .map(|(_, ty)| ty.r#type().unwrap())
                            .collect(),
                    })),
            );
            Ok(())
        }
    }
    root.visit(&mut Phase2TypesVisitor {
        engine: &mut engine,
    });

    // strip off the `Option`s from our types
    // (and in doing so, ensure every node has been given a type)
    let mut root = root
        .upgrade(
            &|r#type: Option<types::TypeId>| -> Result<types::TypeId, &'static str> {
                r#type.ok_or("type data not populated correctly")
            },
        )
        .unwrap();

    struct PopulateFunctionTypesVisitor<'e> {
        engine: &'e mut types::Engine,
    };
    impl<'s, 'e> ASTVisitor<'s, types::TypeId> for PopulateFunctionTypesVisitor<'e> {
        type Error = ();
        fn visit_typename_node(
            &mut self,
            r#type: &mut ast::TypeName<'s, types::TypeId>,
        ) -> Result<(), Self::Error> {
            dbg!(r#type);
            Ok(())
        }
    }
    root.visit(&mut PopulateFunctionTypesVisitor {
        engine: &mut engine,
    });

    println!("{:#?}", root);
}

fn repl_do_parse(tokens: Vec<parse::LexResult<'_>>) -> Option<ast::Function<'_, ()>> {
    use winnow::Parser;
    match parse::function.parse(&tokens) {
        Err(err) => {
            eprintln!("parse error: {:?}", err);
            None
        }
        Ok(n) => Some(n),
    }
}

fn repl_do_lex(line: &str) -> Option<Vec<parse::LexResult<'_>>> {
    let tokens: Result<Vec<parse::LexResult<'_>>, _> = lex::Token::lexer(line)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => Ok(Into::<parse::LexResult>::into((tok, span))),
            Err(()) => Err(span),
        })
        .collect();
    match tokens {
        Ok(tokens) => Some(tokens),
        Err(std::ops::Range { start, end }) => {
            eprintln!("lex error at {start} thru {end}");
            None
        }
    }
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
