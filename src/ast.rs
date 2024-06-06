// TODO move this alias elsewhere
pub(crate) type YarnStr<'s> = byteyarn::YarnBox<'s, str>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Identifier<'s> {
    pub(crate) name: YarnStr<'s>,
}

impl<'s> Identifier<'s> {
    pub(crate) fn new(name: YarnStr<'s>) -> Self {
        Self { name }
    }
}

impl<'s, Y> From<Y> for Identifier<'s>
where
    Y: Into<YarnStr<'s>>,
{
    fn from(value: Y) -> Self {
        Self::new(value.into())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Number {
    pub(crate) val: crate::lex::NumberData,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Function<'s> {
    pub(crate) name: Identifier<'s>,
    pub(crate) args: FunctionArgs<'s>,
    pub(crate) return_type: Type<'s>,
    pub(crate) statements: StatementList<'s>,
}

// TODO make this a proper struct instead probably
pub(crate) type FunctionArgs<'s> = Vec<(Identifier<'s>, Type<'s>)>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Type<'s> {
    Named(Identifier<'s>),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StatementList<'s> {
    pub statements: Vec<Statement<'s>>,
}

impl<'s> FromIterator<Statement<'s>> for StatementList<'s> {
    fn from_iter<T: IntoIterator<Item = Statement<'s>>>(iter: T) -> Self {
        StatementList {
            statements: Vec::from_iter(iter),
        }
    }
}

impl<'s> winnow::stream::Accumulate<Statement<'s>> for StatementList<'s> {
    fn initial(capacity: Option<usize>) -> Self {
        Self {
            statements: Vec::initial(capacity),
        }
    }

    fn accumulate(&mut self, acc: Statement<'s>) {
        self.statements.accumulate(acc);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Statement<'s> {
    Assign {
        target: LValue<'s>,
        value: Expression<'s>,
    },
    Block {
        statements: StatementList<'s>,
    },
    Expr {
        expr: Expression<'s>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum LValue<'s> {
    Variable(Identifier<'s>),
    Field {
        target: Box<LValue<'s>>,
        field_name: Identifier<'s>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Expression<'s> {
    VariableReference(Identifier<'s>),
    Number(Number),
    BinaryInfix {
        lhs: Box<Expression<'s>>,
        op: BinaryInfixOp,
        rhs: Box<Expression<'s>>,
    },
    // TODO should support methods & namespaced functions eventually probably
    FunctionCall {
        function_name: Identifier<'s>,
        args: Vec<Expression<'s>>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum BinaryInfixOp {
    /// addition
    Add,
    /// subtraction
    Subtract,
    /// multiplication
    Multiply,
    /// division
    Divide,
    /// matrix multiplication
    Matmul,
    /// dot product
    Dot,
    /// cross product
    Cross,
}
