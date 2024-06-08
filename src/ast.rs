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
pub(crate) struct Number<Ty> {
    pub(crate) val: crate::lex::NumberData,
    pub(crate) r#type: Ty,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Function<'s, Ty> {
    pub(crate) name: Identifier<'s>,
    pub(crate) args: FunctionArgs<'s, Ty>,
    pub(crate) return_type: Type<'s, Ty>,
    pub(crate) statements: StatementList<'s, Ty>,
    pub(crate) r#type: Ty,
}

// TODO make this a proper struct instead probably
pub(crate) type FunctionArgs<'s, Ty> = Vec<(Identifier<'s>, Type<'s, Ty>)>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Type<'s, Ty> {
    Named { name: Identifier<'s>, r#type: Ty },
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StatementList<'s, Ty> {
    pub statements: Vec<Statement<'s, Ty>>,
}

impl<'s, Ty> FromIterator<Statement<'s, Ty>> for StatementList<'s, Ty> {
    fn from_iter<T: IntoIterator<Item = Statement<'s, Ty>>>(iter: T) -> Self {
        StatementList {
            statements: Vec::from_iter(iter),
        }
    }
}

impl<'s, Ty> winnow::stream::Accumulate<Statement<'s, Ty>> for StatementList<'s, Ty> {
    fn initial(capacity: Option<usize>) -> Self {
        Self {
            statements: Vec::initial(capacity),
        }
    }

    fn accumulate(&mut self, acc: Statement<'s, Ty>) {
        self.statements.accumulate(acc);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Statement<'s, Ty> {
    Assign {
        target: LValue<'s>,
        value: Expression<'s, Ty>,
    },
    Block {
        statements: StatementList<'s, Ty>,
    },
    Expr {
        expr: Expression<'s, Ty>,
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
pub(crate) enum Expression<'s, Ty> {
    VariableReference(VariableReferenceExpr<'s, Ty>),
    Number(Number<Ty>),
    BinaryInfix(BinaryInfixExpr<'s, Ty>),
    // TODO should support methods & namespaced functions eventually probably
    FunctionCall(FunctionCallExpr<'s, Ty>),
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct VariableReferenceExpr<'s, Ty> {
    pub(crate) name: Identifier<'s>,
    pub(crate) r#type: Ty,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct BinaryInfixExpr<'s, Ty> {
    pub(crate) lhs: Box<Expression<'s, Ty>>,
    pub(crate) op: BinaryInfixOp,
    pub(crate) rhs: Box<Expression<'s, Ty>>,
    pub(crate) r#type: Ty,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct FunctionCallExpr<'s, Ty> {
    pub(crate) function_name: Identifier<'s>,
    pub(crate) args: Vec<Expression<'s, Ty>>,
    pub(crate) r#type: Ty,
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

pub(crate) trait ASTVisitor<'s, Ty> {
    type Error;
    fn visit_function(&mut self, node: &mut Function<'s, Ty>) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_statement(&mut self, node: &mut Statement<'s, Ty>) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_expression(&mut self, node: &mut Expression<'s, Ty>) -> Result<(), Self::Error> {
        Ok(())
    }
    fn visit_type(&mut self, node: &mut Type<'s, Ty>) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub(crate) trait Node<Ty> {
    type SelfWithTy<Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E>;
}

pub(crate) trait VisitableNode<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error>;
}

pub(crate) trait DirectlyTypedNode<Ty>: Node<Ty> {
    fn r#type(&self) -> &Ty;
}

pub(crate) trait NodeTypeUpgrader<Source, Dest, Error> {
    fn upgrade(&self, t: Source) -> Result<Dest, Error>;
}

impl<Source, Dest, Error, F> NodeTypeUpgrader<Source, Dest, Error> for F
where
    F: Fn(Source) -> Result<Dest, Error>,
{
    fn upgrade(&self, t: Source) -> Result<Dest, Error> {
        self(t)
    }
}

impl<Ty> Node<Ty> for Number<Ty> {
    type SelfWithTy<Ty2> = Number<Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(Number {
            val: self.val,
            r#type: upgrader.upgrade(self.r#type)?,
        })
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Number<Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        _visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        Ok(())
    }
}

impl<'s, Ty> Node<Ty> for Function<'s, Ty> {
    type SelfWithTy<Ty2> = Function<'s, Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(Function {
            name: self.name,
            args: self
                .args
                .into_iter()
                .map(|(name, ty)| Ok((name, ty.upgrade(upgrader)?)))
                .collect::<Result<_, _>>()?,
            return_type: self.return_type.upgrade(upgrader)?,
            statements: self.statements.upgrade(upgrader)?,
            r#type: upgrader.upgrade(self.r#type)?,
        })
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Function<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_function(self)?;
        visitor.visit_type(&mut self.return_type)?;
        self.args
            .iter_mut()
            .try_for_each(|(_, r#type)| visitor.visit_type(r#type))?;
        self.statements.visit(visitor)?;
        Ok(())
    }
}

impl<'s, Ty> Node<Ty> for Type<'s, Ty> {
    type SelfWithTy<Ty2> = Type<'s, Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        match self {
            Type::Named { name, r#type } => Ok(Type::Named {
                name,
                r#type: upgrader.upgrade(r#type)?,
            }),
        }
    }
}

impl<'s, Ty> Node<Ty> for StatementList<'s, Ty> {
    type SelfWithTy<Ty2> = StatementList<'s, Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(StatementList {
            statements: self
                .statements
                .into_iter()
                .map(|stmt| stmt.upgrade(upgrader))
                .collect::<Result<_, _>>()?,
        })
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for StatementList<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        self.statements
            .iter_mut()
            .try_for_each(|stmt| stmt.visit(visitor))
    }
}

impl<'s, Ty> Node<Ty> for Statement<'s, Ty> {
    type SelfWithTy<Ty2> = Statement<'s, Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(match self {
            Statement::Assign { target, value } => Statement::Assign {
                target,
                value: value.upgrade(upgrader)?,
            },
            Statement::Block { statements } => Statement::Block {
                statements: statements.upgrade(upgrader)?,
            },
            Statement::Expr { expr } => Statement::Expr {
                expr: expr.upgrade(upgrader)?,
            },
        })
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Statement<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_statement(self)?;
        match self {
            Statement::Assign { target, value } => value.visit(visitor),
            Statement::Block { statements } => statements.visit(visitor),
            Statement::Expr { expr } => expr.visit(visitor),
        }
    }
}

impl<'s, Ty> Node<Ty> for Expression<'s, Ty> {
    type SelfWithTy<Ty2> = Expression<'s, Ty2>;
    fn upgrade<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(match self {
            Expression::VariableReference(VariableReferenceExpr { name, r#type }) => {
                Expression::VariableReference(VariableReferenceExpr {
                    name,
                    r#type: upgrader.upgrade(r#type)?,
                })
            }
            Expression::Number(n) => Expression::Number(n.upgrade(upgrader)?),
            Expression::BinaryInfix(BinaryInfixExpr {
                lhs,
                op,
                rhs,
                r#type,
            }) => Expression::BinaryInfix(BinaryInfixExpr {
                lhs: Box::new(lhs.upgrade(upgrader)?),
                op,
                rhs: Box::new(rhs.upgrade(upgrader)?),
                r#type: upgrader.upgrade(r#type)?,
            }),
            Expression::FunctionCall(FunctionCallExpr {
                function_name,
                args,
                r#type,
            }) => Expression::FunctionCall(FunctionCallExpr {
                function_name,
                args: args
                    .into_iter()
                    .map(|n| n.upgrade(upgrader))
                    .collect::<Result<_, _>>()?,
                r#type: upgrader.upgrade(r#type)?,
            }),
        })
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Expression<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_expression(self)?;
        match self {
            Expression::VariableReference { .. } => Ok(()),
            Expression::Number(_) => Ok(()),
            Expression::BinaryInfix(BinaryInfixExpr { lhs, rhs, .. }) => {
                lhs.visit(visitor).and_then(|_| rhs.visit(visitor))
            }
            Expression::FunctionCall(FunctionCallExpr { args, .. }) => {
                args.iter_mut().try_for_each(|expr| expr.visit(visitor))
            }
        }
    }
}

impl<Ty> DirectlyTypedNode<Ty> for Number<Ty> {
    fn r#type(&self) -> &Ty {
        &self.r#type
    }
}

impl<'s, Ty> DirectlyTypedNode<Ty> for Function<'s, Ty> {
    fn r#type(&self) -> &Ty {
        &self.r#type
    }
}

impl<'s, Ty> DirectlyTypedNode<Ty> for Expression<'s, Ty> {
    fn r#type(&self) -> &Ty {
        match self {
            Self::VariableReference(VariableReferenceExpr { r#type, .. })
            | Self::BinaryInfix(BinaryInfixExpr { r#type, .. })
            | Self::FunctionCall(FunctionCallExpr { r#type, .. }) => r#type,
            Self::Number(data) => data.r#type(),
        }
    }
}

impl<'s, Ty> DirectlyTypedNode<Ty> for Type<'s, Ty> {
    fn r#type(&self) -> &Ty {
        match self {
            Type::Named { r#type, .. } => r#type,
        }
    }
}
