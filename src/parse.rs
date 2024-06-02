use crate::{
    ast::{self, YarnStr},
    lex::Token,
};
use winnow::{PResult, Parser};

#[derive(Debug, PartialEq, Clone)]
// TODO rename this better
pub(crate) struct LexResult<'source> {
    pub(crate) token: crate::lex::Token,
    pub(crate) span: logos::Span,
    pub(crate) slice: &'source str,
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

impl<'source, I, E> winnow::Parser<I, LexResult<'source>, E> for Token
where
    I: winnow::stream::Stream<Token = LexResult<'source>> + winnow::stream::StreamIsPartial,
    E: winnow::error::ParserError<I>,
{
    fn parse_next(&mut self, input: &mut I) -> PResult<LexResult<'source>, E> {
        winnow::token::any
            .verify(|t: &LexResult<'source>| t.token == *self)
            .parse_next(input)
    }
}

// ================================================================================

pub(crate) fn identifier<'source>(
    i: &mut &[LexResult<'source>],
) -> PResult<ast::Identifier<'source>> {
    Token::Identifier
        .map(|r| ast::Identifier::new(YarnStr::<'source>::new(r.slice)))
        .parse_next(i)
}

pub(crate) fn number<'source>(i: &mut &[LexResult<'source>]) -> PResult<ast::Number> {
    todo!()
    // Token::Number
    //     .map(|r| {
    //         // (we basically need to fully parse the number again here since the lexer only gives
    //         //  us one overall token for the whole literal)
    //     })
    //     .parse_next(i)
}
