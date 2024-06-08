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
    pub(crate) return_type: TypeName<'s, Ty>,
    pub(crate) statements: StatementList<'s, Ty>,
    pub(crate) r#type: Ty,
}

// TODO make this a proper struct instead probably
pub(crate) type FunctionArgs<'s, Ty> = Vec<(Identifier<'s>, TypeName<'s, Ty>)>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TypeName<'s, Ty> {
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
    /// called for each [`Function`] node in the tree
    fn visit_function_node(&mut self, function: &mut Function<'s, Ty>) -> Result<(), Self::Error> {
        Ok(())
    }
    /// called for each [`Statement`] node in the tree
    fn visit_statement_node(
        &mut self,
        statement: &mut Statement<'s, Ty>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    /// called for each [`Expression`] node in the tree
    fn visit_expression_node(
        &mut self,
        expression: &mut Expression<'s, Ty>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    /// called for each [`TypeName`] node in the tree
    fn visit_typename_node(&mut self, typename: &mut TypeName<'s, Ty>) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub(crate) trait Node<Ty> {
    type SelfWithTy<Ty2>;
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E>;
    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E>;
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
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(Number {
            val: self.val,
            r#type: upgrader.upgrade(self.r#type)?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        inspector(&self.r#type)
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
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(Function {
            name: self.name,
            args: self
                .args
                .into_iter()
                .map(|(name, ty)| Ok((name, ty.upgrade_types(upgrader)?)))
                .collect::<Result<_, _>>()?,
            return_type: self.return_type.upgrade_types(upgrader)?,
            statements: self.statements.upgrade_types(upgrader)?,
            r#type: upgrader.upgrade(self.r#type)?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        self.args
            .iter()
            .try_for_each(|(_, ty)| ty.inspect_types(inspector))?;
        self.return_type.inspect_types(inspector)?;
        self.statements.inspect_types(inspector)?;
        inspector(&self.r#type)?;
        Ok(())
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Function<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_function_node(self)?;
        visitor.visit_typename_node(&mut self.return_type)?;
        self.args
            .iter_mut()
            .try_for_each(|(_, r#type)| visitor.visit_typename_node(r#type))?;
        self.statements.visit(visitor)?;
        Ok(())
    }
}

impl<'s, Ty> Node<Ty> for TypeName<'s, Ty> {
    type SelfWithTy<Ty2> = TypeName<'s, Ty2>;
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        match self {
            TypeName::Named { name, r#type } => Ok(TypeName::Named {
                name,
                r#type: upgrader.upgrade(r#type)?,
            }),
        }
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        inspector(self.r#type())
    }
}

impl<'s, Ty> Node<Ty> for StatementList<'s, Ty> {
    type SelfWithTy<Ty2> = StatementList<'s, Ty2>;
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(StatementList {
            statements: self
                .statements
                .into_iter()
                .map(|stmt| stmt.upgrade_types(upgrader))
                .collect::<Result<_, _>>()?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        self.statements
            .iter()
            .try_for_each(|stmt| stmt.inspect_types(inspector))
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
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
        self,
        upgrader: &U,
    ) -> Result<Self::SelfWithTy<Ty2>, E> {
        Ok(match self {
            Statement::Assign { target, value } => Statement::Assign {
                target,
                value: value.upgrade_types(upgrader)?,
            },
            Statement::Block { statements } => Statement::Block {
                statements: statements.upgrade_types(upgrader)?,
            },
            Statement::Expr { expr } => Statement::Expr {
                expr: expr.upgrade_types(upgrader)?,
            },
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        match self {
            Statement::Assign { target, value } => value.inspect_types(inspector),
            Statement::Block { statements } => statements.inspect_types(inspector),
            Statement::Expr { expr } => expr.inspect_types(inspector),
        }
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Statement<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_statement_node(self)?;
        match self {
            Statement::Assign { target, value } => value.visit(visitor),
            Statement::Block { statements } => statements.visit(visitor),
            Statement::Expr { expr } => expr.visit(visitor),
        }
    }
}

impl<'s, Ty> Node<Ty> for Expression<'s, Ty> {
    type SelfWithTy<Ty2> = Expression<'s, Ty2>;
    fn upgrade_types<Ty2, E, U: NodeTypeUpgrader<Ty, Ty2, E>>(
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
            Expression::Number(n) => Expression::Number(n.upgrade_types(upgrader)?),
            Expression::BinaryInfix(BinaryInfixExpr {
                lhs,
                op,
                rhs,
                r#type,
            }) => Expression::BinaryInfix(BinaryInfixExpr {
                lhs: Box::new(lhs.upgrade_types(upgrader)?),
                op,
                rhs: Box::new(rhs.upgrade_types(upgrader)?),
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
                    .map(|n| n.upgrade_types(upgrader))
                    .collect::<Result<_, _>>()?,
                r#type: upgrader.upgrade(r#type)?,
            }),
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        match self {
            Expression::VariableReference(vr) => inspector(&vr.r#type),
            Expression::Number(num) => num.inspect_types(inspector),
            Expression::BinaryInfix(binfix) => {
                binfix.lhs.inspect_types(inspector)?;
                binfix.rhs.inspect_types(inspector)?;
                inspector(&binfix.r#type)
            }
            Expression::FunctionCall(funccall) => {
                funccall
                    .args
                    .iter()
                    .try_for_each(|arg| arg.inspect_types(inspector))?;
                inspector(&funccall.r#type)
            }
        }
    }
}

impl<'s, Ty> VisitableNode<'s, Ty> for Expression<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_expression_node(self)?;
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

impl<'s, Ty> DirectlyTypedNode<Ty> for TypeName<'s, Ty> {
    fn r#type(&self) -> &Ty {
        match self {
            TypeName::Named { r#type, .. } => r#type,
        }
    }
}
