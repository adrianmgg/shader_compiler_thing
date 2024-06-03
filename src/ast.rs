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

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Number {
    pub(crate) val: crate::lex::NumberData,
}

#[derive(Debug)]
pub(crate) struct Function<'s> {
    pub(crate) name: Identifier<'s>,
    pub(crate) args: FunctionArgs<'s>,
    pub(crate) return_type: Type<'s>,
    pub(crate) statements: StatementList<'s>,
}

// TODO make this a proper struct instead probably
pub(crate) type FunctionArgs<'s> = Vec<(Identifier<'s>, Type<'s>)>;

#[derive(Debug)]
pub(crate) enum Type<'s> {
    Named(Identifier<'s>),
}

#[derive(Debug)]
pub(crate) struct StatementList<'s> {
    pub statements: Vec<Statement<'s>>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) struct LValue<'s> {
    _todo: std::marker::PhantomData<&'s ()>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
