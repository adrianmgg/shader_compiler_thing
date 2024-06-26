pub(crate) mod helpers;

use crate::{
    ast,
    lex::{self, Token},
};
use helpers::noleftrec;
use winnow::{
    combinator::{
        alt, delimited, dispatch, empty, fail, preceded, repeat, separated, separated_pair, seq,
        terminated, trace,
    },
    PResult, Parser,
};

#[derive(Debug, PartialEq, Clone)]
// TODO rename this better
pub(crate) struct LexResult<'source> {
    pub(crate) token: Token<'source>,
    pub(crate) span: logos::Span,
}

impl<'source> From<(Token<'source>, logos::Span)> for LexResult<'source> {
    fn from(value: (Token<'source>, logos::Span)) -> Self {
        LexResult {
            token: value.0,
            span: value.1,
        }
    }
}

// ================================================================================

/*
// TODO give this a better name
pub(crate) struct LexerDriver<'source> {
    lexer: logos::Lexer<'source, crate::lex::Token>,
}

#[derive(Debug, Clone)]
pub(crate) struct LexerDriverCheckpoint<'source> {
    extras: <crate::lex::Token as logos::Logos<'source>>::Extras,
}

impl<'source> winnow::stream::Offset for LexerDriverCheckpoint<'source> {
    fn offset_from(&self, start: &Self) -> usize {
        todo!()
    }
}

impl<'source> winnow::stream::Stream for LexerDriver<'source> {
    type Token = LexResult;
    type Checkpoint = LexerDriverCheckpoint<'source>;
}
*/

// ================================================================================

// TODO probably factor `&mut &[LexResult<'source>]` into a type alias or smth so we can swap it
//      out easier
// TODO ^ same goes for our PResult since that'll probably change once weve got a custom err type
//      maybe

pub(crate) mod token {
    use winnow::{combinator::trace, PResult, Parser};

    use crate::{ast, lex::Token};

    use super::LexResult;

    pub(crate) fn identifier<'source, I, E>(i: &mut I) -> PResult<ast::Identifier<'source>, E>
    where
        I: winnow::stream::Stream<Token = LexResult<'source>> + winnow::stream::StreamIsPartial,
        E: winnow::error::ParserError<I>,
    {
        trace(
            "token::identifier",
            winnow::token::any.verify_map(|t: LexResult<'source>| match t.token {
                Token::Identifier(yarn) => Some(ast::Identifier::new(yarn)),
                _ => None,
            }),
        )
        .parse_next(i)
    }

    pub(crate) fn number<'source, I, E>(i: &mut I) -> PResult<ast::Number<()>, E>
    where
        I: winnow::stream::Stream<Token = LexResult<'source>> + winnow::stream::StreamIsPartial,
        E: winnow::error::ParserError<I>,
    {
        trace(
            "token::number",
            winnow::token::any.verify_map(|t: LexResult<'source>| match t.token {
                Token::Number(n) => Some(ast::Number { val: n, r#type: () }),
                _ => None,
            }),
        )
        .parse_next(i)
    }

    crate::lex::for_each_simple_token!({
        ($(($fnname:ident, $token:ident, $_:literal)),*) => {$(
            #[allow(unused)]
            pub(crate) fn $fnname<'source, I, E>(i: &mut I) -> PResult<(), E>
            where
                I: winnow::stream::Stream<Token = LexResult<'source>>
                    + winnow::stream::StreamIsPartial,
                E: winnow::error::ParserError<I>,
            {
                // TODO is there a way i should be doing this other than with token::any
                trace(
                    concat!("token::", stringify!($fnname)),
                    winnow::token::any.verify_map(|t: LexResult<'source>| match t.token {
                        crate::lex::Token::$token => Some(()),
                        _ => None,
                    })
                )
                .parse_next(i)
            }
        )*};
    });

    #[cfg(test)]
    mod tests {
        crate::lex::for_each_simple_token!({
            ($(($fnname:ident, $token:ident, $tokenstr:literal)),*) => {
                $($crate::parse::tests::should_parse_test!(
                    $fnname,
                    $crate::parse::token::$fnname,
                    $tokenstr
                );)*
            };
        });
        use crate::ast;
        crate::parse::tests::should_parse_to_test!(
            identifier,
            crate::parse::token::identifier,
            "foo",
            // (qualifying the call like this since this test shouldn't pass if for some reason the
            //  return type wasn't an ast::Identifier but could still do `"foo".into()`)
            Ok(Into::<ast::Identifier>::into("foo"))
        );
        crate::parse::tests::should_parse_to_test!(
            number,
            crate::parse::token::number,
            "10x12345u32",
            Ok(ast::Number {
                val: 12345u32.into(),
                r#type: (),
            })
        );
    }
}

pub(crate) fn function<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::Function<'source, (), ()>> {
    use ast::Function;
    trace(
        "function",
        seq! {Function{
            name: token::identifier,
            _: token::openparen,
            args: function_args,
            _: token::closeparen,
            _: token::rightarrow,
            return_type: r#type,
            _: token::opencurly,
            statements: statement_list,
            _: token::closecurly,
            r#type: (),
            scope: (),
        }},
    )
    .parse_next(i)
}

fn function_args<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::FunctionArgs<'source, ()>> {
    trace(
        "function_args",
        separated(
            0..,
            separated_pair(token::identifier, token::colon, r#type),
            token::comma,
        ),
    )
    .parse_next(i)
}

pub(crate) fn r#type<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::TypeName<'source, ()>> {
    trace(
        "type",
        alt((token::identifier.map(|id| ast::TypeName::Named {
            name: id,
            r#type: (),
        }),)),
    )
    .parse_next(i)
}

pub(crate) fn statement_list<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::StatementList<'source, (), ()>> {
    trace(
        "statement_list",
        repeat(0.., terminated(statement, token::semicolon)),
    )
    .parse_next(i)
}

pub(crate) fn statement<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::Statement<'source, (), ()>> {
    trace(
        "statement",
        alt((
            seq!(ast::Statement::Assign {
                target: lvalue,
                _: token::singleequals,
                value: expression,
            }),
            seq!(ast::Statement::Block {
                _: token::opencurly,
                statements: statement_list,
                _: token::closecurly,
                scope: (),
            }),
            seq!(ast::Statement::Expr { expr: expression }),
        )),
    )
    .parse_next(i)
}

pub(crate) fn lvalue<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::LValue<'source>> {
    trace(
        "lvalue",
        noleftrec(
            preceded(token::period, token::identifier),
            token::identifier,
            ast::LValue::Variable,
            |target, field_name| ast::LValue::Field {
                target: Box::new(target),
                field_name,
            },
        ),
    )
    .parse_next(i)
}

pub fn expression<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Expression<'source, ()>> {
    fn expr_prec0<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Expression<'source, ()>> {
        use expr_prec1 as next;
        trace(
            "expression::expr_prec0",
            noleftrec(
                (
                    alt((
                        token::plus.value(ast::BinaryInfixOp::Add),
                        token::minus.value(ast::BinaryInfixOp::Subtract),
                    )),
                    next,
                ),
                next,
                std::convert::identity,
                |lhs, (op, rhs)| {
                    ast::Expression::BinaryInfix(ast::BinaryInfixExpr {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                        r#type: (),
                    })
                },
            ),
        )
        .parse_next(i)
    }
    fn expr_prec1<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Expression<'source, ()>> {
        use expr_prec2 as next;
        trace(
            "expression::expr_prec1",
            noleftrec(
                (
                    alt((
                        token::asterisk.value(ast::BinaryInfixOp::Multiply),
                        token::slash.value(ast::BinaryInfixOp::Divide),
                        token::atsign.value(ast::BinaryInfixOp::Matmul),
                    )),
                    next,
                ),
                next,
                std::convert::identity,
                |lhs, (op, rhs)| {
                    ast::Expression::BinaryInfix(ast::BinaryInfixExpr {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                        r#type: (),
                    })
                },
            ),
        )
        .parse_next(i)
    }
    fn expr_prec2<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Expression<'source, ()>> {
        use expr_prec0 as start;
        trace(
            "expression::expr_prec2",
            alt((
                trace(
                    "function call",
                    seq! {ast::FunctionCallExpr{
                        function_name: token::identifier,
                        _: token::openparen,
                        // TODO support trailing comma
                        args: separated(0.., expression, token::comma),
                        _: token::closeparen,
                        r#type: (),
                    }}
                    .map(ast::Expression::FunctionCall),
                ),
                trace(
                    "variable reference",
                    token::identifier.map(|name| {
                        ast::Expression::VariableReference(ast::VariableReferenceExpr {
                            name,
                            r#type: (),
                        })
                    }),
                ),
                trace("number literal", token::number.map(ast::Expression::Number)),
                trace(
                    "expression in parens",
                    delimited(token::openparen, start, token::closeparen),
                ),
            )),
        )
        .parse_next(i)
    }
    trace("expression", expr_prec0).parse_next(i)
}

pub fn document<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Document<'source, (), ()>> {
    enum TopLevelAcceptableNode<'a, Ty, Scope> {
        Function(ast::Function<'a, Ty, Scope>),
    }

    repeat(
        0..,
        alt((
            //
            function.map(TopLevelAcceptableNode::Function),
        )),
    )
    .fold(
        || ast::Document {
            functions: Default::default(),
            scope: (),
        },
        |mut doc, item| {
            match item {
                TopLevelAcceptableNode::Function(f) => doc.functions.push(f),
            }
            doc
        },
    )
    .parse_next(i)
}

#[cfg(test)]
mod tests {
    macro_rules! should_parse_test {
        ($testname:ident, $parser:expr, $teststr:literal) => {
            #[test]
            fn $testname() -> winnow::PResult<()> {
                use logos::Logos;
                use winnow::Parser;
                let tokens: Vec<crate::parse::LexResult<'_>> = crate::lex::Token::lexer($teststr)
                    .spanned()
                    .map(|(tok, range)| (tok.expect("lexing failed"), range).into())
                    .collect();
                $parser.map(|_| ()).parse_next(&mut tokens.as_slice())
            }
        };
    }
    pub(crate) use should_parse_test;
    macro_rules! should_parse_to_test {
        ($testname:ident, $parser:expr, $teststr:literal, $expected_result:expr, $errtype:path) => {
            #[test]
            fn $testname() {
                use logos::Logos;
                use winnow::Parser;
                let tokens: Vec<crate::parse::LexResult<'_>> = crate::lex::Token::lexer($teststr)
                    .spanned()
                    .map(|(tok, range)| (tok.expect("lexing failed"), range).into())
                    .collect();
                let parse_result: Result<_, winnow::error::ParseError<_, $errtype>> =
                    $parser.parse(tokens.as_slice());
                assert_eq!(parse_result, $expected_result);
            }
        };
        // default $errtype to PResult's default
        ($testname:ident, $parser:expr, $teststr:literal, $expected_result:expr) => {
            $crate::parse::tests::should_parse_to_test!(
                $testname,
                $parser,
                $teststr,
                $expected_result,
                winnow::error::ContextError
            );
        };
    }
    pub(crate) use should_parse_to_test;

    use crate::{ast, parse};

    should_parse_to_test!(
        type_name,
        parse::r#type,
        "foo",
        Ok(ast::TypeName::Named {
            name: "foo".into(),
            r#type: ()
        })
    );

    should_parse_to_test!(
        function,
        parse::function,
        "foo(a: A, b: B) -> R { }",
        Ok(ast::Function {
            name: "foo".into(),
            args: vec![
                (
                    "a".into(),
                    ast::TypeName::Named {
                        name: "A".into(),
                        r#type: ()
                    }
                ),
                (
                    "b".into(),
                    ast::TypeName::Named {
                        name: "B".into(),
                        r#type: ()
                    }
                )
            ],
            return_type: ast::TypeName::Named {
                name: "R".into(),
                r#type: ()
            },
            statements: ast::StatementList {
                statements: vec![],
                scope: ()
            },
            r#type: (),
            scope: (),
        })
    );

    should_parse_to_test!(expr_precedence, parse::expression, "a + b * c / d - e", {
        use ast::{
            BinaryInfixExpr,
            BinaryInfixOp::{Add, Divide, Multiply, Subtract},
            Expression::{BinaryInfix, VariableReference},
            VariableReferenceExpr,
        };
        Ok(BinaryInfix(BinaryInfixExpr {
            lhs: BinaryInfix(BinaryInfixExpr {
                lhs: VariableReference(VariableReferenceExpr {
                    name: "a".into(),
                    r#type: (),
                })
                .into(),
                op: Add,
                rhs: BinaryInfix(BinaryInfixExpr {
                    lhs: BinaryInfix(BinaryInfixExpr {
                        lhs: VariableReference(VariableReferenceExpr {
                            name: "b".into(),
                            r#type: (),
                        })
                        .into(),
                        op: Multiply,
                        rhs: VariableReference(VariableReferenceExpr {
                            name: "c".into(),
                            r#type: (),
                        })
                        .into(),
                        r#type: (),
                    })
                    .into(),
                    op: Divide,
                    rhs: VariableReference(VariableReferenceExpr {
                        name: "d".into(),
                        r#type: (),
                    })
                    .into(),
                    r#type: (),
                })
                .into(),
                r#type: (),
            })
            .into(),
            op: Subtract,
            rhs: VariableReference(VariableReferenceExpr {
                name: "e".into(),
                r#type: (),
            })
            .into(),
            r#type: (),
        }))
    });

    should_parse_to_test!(
        field_assignment,
        parse::statement,
        "a.b.c = d",
        Ok(ast::Statement::Assign {
            target: ast::LValue::Field {
                target: ast::LValue::Field {
                    target: ast::LValue::Variable("a".into()).into(),
                    field_name: "b".into(),
                }
                .into(),
                field_name: "c".into()
            },
            value: ast::Expression::VariableReference(ast::VariableReferenceExpr {
                name: "d".into(),
                r#type: ()
            }),
        })
    );
}
