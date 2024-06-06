use std::{io::Error, marker::PhantomData};

use winnow::{combinator::trace, error::ErrMode, PResult, Parser};

// TODO wait actually we can probably remove the alt case logic from this i think?

/// helper for avoiding infinite recursion with left-recursive parsers.
///
/// # adapting something to use with this
///
/// given a grammar like e.g.
/// ```text
/// foo = foo a b
///     | foo c d e
///     | x y z
/// ```
/// first, group together all the right hand sides
/// ```text
/// rhs = a b
///     | c d e
/// ```
/// then, separate out the 'alt' case (the one(s) not prefixed with `foo`)
/// ```text
/// alt = x y z
/// ```
/// (the grammar now looks like this)
/// ```text
/// foo = foo rhs
///     | alt
/// ```
pub(crate) fn noleftrec<
    Input,
    Error,
    ORhs,
    OAlt,
    OSelf,
    PRhs,
    PAlt,
    FNRewriteLeaf,
    FNRewriteBranch,
>(
    rhs_parser: PRhs,
    alt_parser: PAlt,
    rewrite_leaf: FNRewriteLeaf,
    rewrite_branch: FNRewriteBranch,
) -> impl Parser<Input, OSelf, Error>
where
    PRhs: Parser<Input, ORhs, Error>,
    PAlt: Parser<Input, OAlt, Error>,
    FNRewriteLeaf: Fn(OAlt) -> OSelf,
    FNRewriteBranch: Fn(OSelf, ORhs) -> OSelf,
    Input: winnow::stream::Stream,
{
    trace(
        "noleftrec",
        NoLeftRec::new(rhs_parser, alt_parser, rewrite_leaf, rewrite_branch),
    )
}

pub(crate) struct NoLeftRec<
    Input,
    Error,
    ORhs,
    OAlt,
    OSelf,
    PRhs,
    PAlt,
    FNRewriteLeaf,
    FNRewriteBranch,
> where
    PRhs: Parser<Input, ORhs, Error>,
    PAlt: Parser<Input, OAlt, Error>,
    FNRewriteLeaf: Fn(OAlt) -> OSelf,
    FNRewriteBranch: Fn(OSelf, ORhs) -> OSelf,
{
    rhs_parser: PRhs,
    alt_parser: PAlt,
    rewrite_leaf: FNRewriteLeaf,
    rewrite_branch: FNRewriteBranch,
    phantom: PhantomData<(Input, Error, ORhs, OAlt, OSelf)>,
}

// TODO wait now that we're not doing impls for these they can just be tuples / Option'd tuples
enum NLRInnerOutput<ORhs> {
    NotEnd(ORhs, Box<Self>),
    End,
}

struct NLROuterOutput<ORhs, OAlt>(OAlt, NLRInnerOutput<ORhs>);

impl<Input, Error, ORhs, OAlt, OSelf, PRhs, PAlt, FNRewriteLeaf, FNRewriteBranch>
    NoLeftRec<Input, Error, ORhs, OAlt, OSelf, PRhs, PAlt, FNRewriteLeaf, FNRewriteBranch>
where
    PRhs: Parser<Input, ORhs, Error>,
    PAlt: Parser<Input, OAlt, Error>,
    FNRewriteLeaf: Fn(OAlt) -> OSelf,
    FNRewriteBranch: Fn(OSelf, ORhs) -> OSelf,
{
    fn new(
        rhs_parser: PRhs,
        alt_parser: PAlt,
        rewrite_leaf: FNRewriteLeaf,
        rewrite_branch: FNRewriteBranch,
    ) -> Self {
        Self {
            rhs_parser,
            alt_parser,
            rewrite_leaf,
            rewrite_branch,
            phantom: PhantomData,
        }
    }

    // TODO wait do we need to do any manual checkpoint/backtrack handling here b/c/o splitting
    //      this out to its own thing? or is it all good as is
    fn parse_next_inner(&mut self, input: &mut Input) -> PResult<NLRInnerOutput<ORhs>, Error> {
        match self.rhs_parser.parse_next(input) {
            Err(ErrMode::Backtrack(_)) => Ok(NLRInnerOutput::End),
            Err(e) => Err(e),
            Ok(rhs) => match self.parse_next_inner(input) {
                Err(ErrMode::Backtrack(_)) => Ok(NLRInnerOutput::End),
                Err(e) => Err(e),
                Ok(next_match) => Ok(NLRInnerOutput::NotEnd(rhs, Box::new(next_match))),
            },
        }
    }

    fn rewrite_inner_output(&self, inner_output: NLRInnerOutput<ORhs>, lhs: OSelf) -> OSelf {
        match inner_output {
            NLRInnerOutput::End => lhs,
            NLRInnerOutput::NotEnd(rhs, next) => {
                self.rewrite_inner_output(*next, (self.rewrite_branch)(lhs, rhs))
            }
        }
    }

    fn rewrite_outer_output(&self, outer_output: NLROuterOutput<ORhs, OAlt>) -> OSelf {
        self.rewrite_inner_output(outer_output.1, (self.rewrite_leaf)(outer_output.0))
    }
}

impl<Input, Error, ORhs, OAlt, OSelf, PRhs, PAlt, FNRewriteLeaf, FNRewriteBranch>
    Parser<Input, OSelf, Error>
    for NoLeftRec<Input, Error, ORhs, OAlt, OSelf, PRhs, PAlt, FNRewriteLeaf, FNRewriteBranch>
where
    PRhs: Parser<Input, ORhs, Error>,
    PAlt: Parser<Input, OAlt, Error>,
    FNRewriteLeaf: Fn(OAlt) -> OSelf,
    FNRewriteBranch: Fn(OSelf, ORhs) -> OSelf,
{
    fn parse_next(&mut self, input: &mut Input) -> PResult<OSelf, Error> {
        let intermediate = NLROuterOutput(
            self.alt_parser.parse_next(input)?,
            self.parse_next_inner(input)?,
        );
        Ok(self.rewrite_outer_output(intermediate))
    }
}
