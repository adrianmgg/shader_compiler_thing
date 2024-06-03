use crate::{
    ast::{self, YarnStr},
    lex::Token,
};
use winnow::{combinator::seq, PResult, Parser};

#[derive(Debug, PartialEq, Clone)]
// TODO rename this better
pub(crate) struct LexResult<'source> {
    pub(crate) token: crate::lex::Token<'source>,
    pub(crate) span: logos::Span,
}

impl winnow::stream::ContainsToken<LexResult<'_>> for LexResult<'_> {
    #[inline]
    fn contains_token(&self, token: LexResult) -> bool {
        *self == token
    }
}

impl winnow::stream::ContainsToken<LexResult<'_>> for &'_ [LexResult<'_>] {
    #[inline]
    fn contains_token(&self, token: LexResult) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<LexResult<'_>> for &'_ [LexResult<'_>; LEN] {
    #[inline]
    fn contains_token(&self, token: LexResult) -> bool {
        self.iter().any(|t| *t == token)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<LexResult<'_>> for [LexResult<'_>; LEN] {
    #[inline]
    fn contains_token(&self, token: LexResult) -> bool {
        self.iter().any(|t| *t == token)
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
    use winnow::{PResult, Parser};

    use crate::{ast, lex::Token};

    use super::LexResult;

    pub(crate) fn identifier<'source, I, E>(i: &mut I) -> PResult<ast::Identifier<'source>, E>
    where
        I: winnow::stream::Stream<Token = LexResult<'source>> + winnow::stream::StreamIsPartial,
        E: winnow::error::ParserError<I>,
    {
        winnow::token::any
            .verify_map(|t: LexResult<'source>| match t.token {
                Token::Identifier(yarn) => Some(ast::Identifier::new(yarn)),
                _ => None,
            })
            .parse_next(i)
    }

    pub(crate) fn number<'source, I, E>(i: &mut I) -> PResult<ast::Number, E>
    where
        I: winnow::stream::Stream<Token = LexResult<'source>> + winnow::stream::StreamIsPartial,
        E: winnow::error::ParserError<I>,
    {
        winnow::token::any
            .verify_map(|t: LexResult<'_>| match t.token {
                Token::Number(val) => Some(ast::Number { val }),
                _ => None,
            })
            .parse_next(i)
    }

    macro_rules! simple_token_parsefn {
        ($token:path, $fnname:ident) => {
            pub(crate) fn $fnname<'source, I, E>(i: &mut I) -> PResult<(), E>
            where
                I: winnow::stream::Stream<Token = LexResult<'source>>
                    + winnow::stream::StreamIsPartial,
                E: winnow::error::ParserError<I>,
            {
                // TODO is there a way i should be doing this other than with token::any
                winnow::token::any
                    .verify_map(|t: LexResult<'source>| match t.token {
                        $token => Some(()),
                        _ => None,
                    })
                    .parse_next(i)
            }
        };
    }

    simple_token_parsefn!(Token::Comma, comma);
    simple_token_parsefn!(Token::Colon, colon);
    simple_token_parsefn!(Token::Semicolon, semicolon);
    simple_token_parsefn!(Token::RightArrow, rightarrow);
    simple_token_parsefn!(Token::SingleEquals, singleequals);
    simple_token_parsefn!(Token::DoubleEquals, doubleequals);
    simple_token_parsefn!(Token::Plus, plus);
    simple_token_parsefn!(Token::Minus, minus);
    simple_token_parsefn!(Token::Asterisk, asterisk);
    simple_token_parsefn!(Token::Slash, slash);
    simple_token_parsefn!(Token::AtSign, atsign);
    simple_token_parsefn!(Token::OpenBracket, openbracket);
    simple_token_parsefn!(Token::CloseBracket, closebracket);
    simple_token_parsefn!(Token::OpenCurly, opencurly);
    simple_token_parsefn!(Token::CloseCurly, closecurly);
    simple_token_parsefn!(Token::OpenParen, openparen);
    simple_token_parsefn!(Token::CloseParen, closeparen);
}

pub(crate) fn function<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Function<'source>> {
    use ast::Function;
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
    }}
    .parse_next(i)
}

fn function_args<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::FunctionArgs<'source>> {
    todo!()
}

pub(crate) fn r#type<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Type<'source>> {
    todo!()
}

pub(crate) fn statement_list<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::StatementList<'source>> {
    todo!()
}
