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
pub(crate) struct Function<'s, Ty, Scope> {
    pub(crate) name: Identifier<'s>,
    pub(crate) args: FunctionArgs<'s, Ty>,
    pub(crate) return_type: TypeName<'s, Ty>,
    pub(crate) statements: StatementList<'s, Ty, Scope>,
    pub(crate) r#type: Ty,
    pub(crate) scope: Scope,
}

// TODO make this a proper struct instead probably
pub(crate) type FunctionArgs<'s, Ty> = Vec<(Identifier<'s>, TypeName<'s, Ty>)>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum TypeName<'s, Ty> {
    Named { name: Identifier<'s>, r#type: Ty },
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct StatementList<'s, Ty, Scope> {
    pub(crate) statements: Vec<Statement<'s, Ty, Scope>>,
    pub(crate) scope: Scope,
}

impl<'s, Ty, Scope> winnow::stream::Accumulate<Statement<'s, Ty, Scope>>
    for StatementList<'s, Ty, Scope>
where
    Scope: Default,
{
    fn initial(capacity: Option<usize>) -> Self {
        Self {
            statements: Vec::initial(capacity),
            scope: Default::default(),
        }
    }

    fn accumulate(&mut self, acc: Statement<'s, Ty, Scope>) {
        self.statements.accumulate(acc);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Statement<'s, Ty, Scope> {
    Assign {
        target: LValue<'s>,
        value: Expression<'s, Ty>,
    },
    Block {
        statements: StatementList<'s, Ty, Scope>,
        scope: Scope,
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

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Document<'s, Ty, Scope> {
    pub(crate) scope: Scope,
    pub(crate) functions: Vec<Function<'s, Ty, Scope>>,
}

pub(crate) trait ASTVisitor<'s, Ty, Scope> {
    type Error;
    /// called for each [`Function`] node in the tree
    fn visit_function_node(
        &mut self,
        function: &mut Function<'s, Ty, Scope>,
        scope: &Scope,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    /// called for each [`Statement`] node in the tree
    fn visit_statement_node(
        &mut self,
        statement: &mut Statement<'s, Ty, Scope>,
        scope: &Scope,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    /// called for each [`Expression`] node in the tree
    fn visit_expression_node(
        &mut self,
        expression: &mut Expression<'s, Ty>,
        scope: &Scope,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    /// called for each [`TypeName`] node in the tree
    fn visit_typename_node(
        &mut self,
        typename: &mut TypeName<'s, Ty>,
        scope: &Scope,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub(crate) trait Node<Ty, Scope> {
    type SelfWithUpgrade<NewTy, NewScope>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E>;

    fn upgrade_types<NewTy, E, TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>>(
        self,
        type_upgrader: &mut TypeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, Scope>, E>
    where
        Self: std::marker::Sized,
    {
        self.upgrade(type_upgrader, &mut Ok)
    }

    fn upgrade_scopes<NewScope, E, ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>>(
        self,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<Ty, NewScope>, E>
    where
        Self: std::marker::Sized,
    {
        self.upgrade(&mut Ok, scope_upgrader)
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E>;
}

pub(crate) trait VisitableNode<'s, Ty, Scope> {
    fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        visitor: &mut Visitor,
        scope: &Scope,
    ) -> Result<(), Visitor::Error>;
}

pub(crate) trait DirectlyTypedNode<Ty> {
    fn r#type(&self) -> &Ty;
}

pub(crate) trait NodeUpgrader<Source, Dest, Error> {
    fn upgrade(&self, t: Source) -> Result<Dest, Error>;
}

impl<Source, Dest, Error, F> NodeUpgrader<Source, Dest, Error> for F
where
    F: Fn(Source) -> Result<Dest, Error>,
{
    fn upgrade(&self, t: Source) -> Result<Dest, Error> {
        self(t)
    }
}

impl<Ty, Scope> Node<Ty, Scope> for Number<Ty> {
    type SelfWithUpgrade<NewTy, NewScope> = Number<NewTy>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        Ok(Number {
            val: self.val,
            r#type: type_upgrader(self.r#type)?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        inspector(&self.r#type)
    }
}

impl<'s, Ty, Scope> VisitableNode<'s, Ty, Scope> for Number<Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        _visitor: &mut Visitor,
        scope: &Scope,
    ) -> Result<(), Visitor::Error> {
        Ok(())
    }
}

impl<'s, Ty, Scope> Node<Ty, Scope> for Function<'s, Ty, Scope> {
    type SelfWithUpgrade<NewTy, NewScope> = Function<'s, NewTy, NewScope>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        Ok(Function {
            name: self.name,
            args: self
                .args
                .into_iter()
                .map(|(name, ty)| Ok((name, ty.upgrade(type_upgrader, scope_upgrader)?)))
                .collect::<Result<_, _>>()?,
            return_type: self.return_type.upgrade(type_upgrader, scope_upgrader)?,
            statements: self.statements.upgrade(type_upgrader, scope_upgrader)?,
            r#type: type_upgrader(self.r#type)?,
            scope: scope_upgrader(self.scope)?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        self.args
            .iter()
            .try_for_each(|(_, ty)| Node::<Ty, Scope>::inspect_types(ty, inspector))?;
        Node::<Ty, Scope>::inspect_types(&self.return_type, inspector)?;
        self.statements.inspect_types(inspector)?;
        inspector(&self.r#type)?;
        Ok(())
    }
}

impl<'s, Ty, Scope> VisitableNode<'s, Ty, Scope> for Function<'s, Ty, Scope> {
    fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        visitor: &mut Visitor,
        scope: &Scope,
    ) -> Result<(), Visitor::Error> {
        // visit ourself with outer scope, children with our scope
        visitor.visit_function_node(self, scope)?;
        visitor.visit_typename_node(&mut self.return_type, &self.scope)?;
        self.args
            .iter_mut()
            .try_for_each(|(_, r#type)| visitor.visit_typename_node(r#type, &self.scope))?;
        self.statements.visit(visitor, &self.scope)?;
        Ok(())
    }
}

impl<'s, Ty, Scope> Node<Ty, Scope> for TypeName<'s, Ty> {
    type SelfWithUpgrade<NewTy, NewScope> = TypeName<'s, NewTy>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        match self {
            TypeName::Named { name, r#type } => Ok(TypeName::Named {
                name,
                r#type: type_upgrader(r#type)?,
            }),
        }
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        inspector(self.r#type())
    }
}

impl<'s, Ty, Scope> Node<Ty, Scope> for StatementList<'s, Ty, Scope> {
    type SelfWithUpgrade<NewTy, NewScope> = StatementList<'s, NewTy, NewScope>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        Ok(StatementList {
            statements: self
                .statements
                .into_iter()
                .map(|stmt| stmt.upgrade(type_upgrader, scope_upgrader))
                .collect::<Result<_, _>>()?,
            scope: scope_upgrader(self.scope)?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        self.statements
            .iter()
            .try_for_each(|stmt| stmt.inspect_types(inspector))
    }
}

impl<'s, Ty, Scope> VisitableNode<'s, Ty, Scope> for StatementList<'s, Ty, Scope> {
    fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        visitor: &mut Visitor,
        scope: &Scope,
    ) -> Result<(), Visitor::Error> {
        self.statements
            .iter_mut()
            .try_for_each(|stmt| stmt.visit(visitor, &self.scope))
    }
}

impl<'s, Ty, Scope> Node<Ty, Scope> for Statement<'s, Ty, Scope> {
    type SelfWithUpgrade<NewTy, NewScope> = Statement<'s, NewTy, NewScope>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        Ok(match self {
            Statement::Assign { target, value } => Statement::Assign {
                target,
                value: value.upgrade(type_upgrader, scope_upgrader)?,
            },
            Statement::Block { statements, scope } => Statement::Block {
                statements: statements.upgrade(type_upgrader, scope_upgrader)?,
                scope: scope_upgrader(scope)?,
            },
            Statement::Expr { expr } => Statement::Expr {
                expr: expr.upgrade(type_upgrader, scope_upgrader)?,
            },
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        match self {
            Statement::Assign { target, value } => {
                Node::<_, Scope>::inspect_types(value, inspector)
            }
            Statement::Block { statements, scope } => statements.inspect_types(inspector),
            Statement::Expr { expr } => Node::<_, Scope>::inspect_types(expr, inspector),
        }
    }
}

impl<'s, Ty, Scope> VisitableNode<'s, Ty, Scope> for Statement<'s, Ty, Scope> {
    fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        visitor: &mut Visitor,
        scope: &Scope,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_statement_node(self, scope)?;
        match self {
            Statement::Assign { target, value } => value.visit(visitor, scope),
            Statement::Block {
                statements,
                scope: block_scope,
            } => statements.visit(visitor, block_scope),
            Statement::Expr { expr } => expr.visit(visitor, scope),
        }
    }
}

impl<'s, Ty, Scope> Node<Ty, Scope> for Expression<'s, Ty> {
    type SelfWithUpgrade<NewTy, NewScope> = Expression<'s, NewTy>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        Ok(match self {
            Expression::VariableReference(VariableReferenceExpr { name, r#type }) => {
                Expression::VariableReference(VariableReferenceExpr {
                    name,
                    r#type: type_upgrader(r#type)?,
                })
            }
            Expression::Number(n) => Expression::Number(n.upgrade(type_upgrader, scope_upgrader)?),
            Expression::BinaryInfix(BinaryInfixExpr {
                lhs,
                op,
                rhs,
                r#type,
            }) => Expression::BinaryInfix(BinaryInfixExpr {
                lhs: Box::new(lhs.upgrade(type_upgrader, scope_upgrader)?),
                op,
                rhs: Box::new(rhs.upgrade(type_upgrader, scope_upgrader)?),
                r#type: type_upgrader(r#type)?,
            }),
            Expression::FunctionCall(FunctionCallExpr {
                function_name,
                args,
                r#type,
            }) => Expression::FunctionCall(FunctionCallExpr {
                function_name,
                args: args
                    .into_iter()
                    .map(|n| n.upgrade(type_upgrader, scope_upgrader))
                    .collect::<Result<_, _>>()?,
                r#type: type_upgrader(r#type)?,
            }),
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        match self {
            Expression::VariableReference(vr) => inspector(&vr.r#type),
            Expression::Number(num) => Node::<_, Scope>::inspect_types(num, inspector),
            Expression::BinaryInfix(binfix) => {
                Node::<_, Scope>::inspect_types(&*binfix.lhs, inspector)?;
                Node::<_, Scope>::inspect_types(&*binfix.rhs, inspector)?;
                inspector(&binfix.r#type)
            }
            Expression::FunctionCall(funccall) => {
                funccall
                    .args
                    .iter()
                    .try_for_each(|arg| Node::<_, Scope>::inspect_types(arg, inspector))?;
                inspector(&funccall.r#type)
            }
        }
    }
}

impl<'s, Ty, Scope> VisitableNode<'s, Ty, Scope> for Expression<'s, Ty> {
    fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        visitor: &mut Visitor,
        scope: &Scope,
    ) -> Result<(), Visitor::Error> {
        visitor.visit_expression_node(self, scope)?;
        match self {
            Expression::VariableReference { .. } => Ok(()),
            Expression::Number(_) => Ok(()),
            Expression::BinaryInfix(BinaryInfixExpr { lhs, rhs, .. }) => lhs
                .visit(visitor, scope)
                .and_then(|_| rhs.visit(visitor, scope)),
            Expression::FunctionCall(FunctionCallExpr { args, .. }) => args
                .iter_mut()
                .try_for_each(|expr| expr.visit(visitor, scope)),
        }
    }
}

impl<'s, Ty, Scope> Document<'s, Ty, Scope> {
    // this is NOT done as a `VisitableNode` impl, since there's no scope to pass in here at the top
    // level
    pub(crate) fn visit<Visitor: ASTVisitor<'s, Ty, Scope>>(
        &mut self,
        visitor: &mut Visitor,
    ) -> Result<(), Visitor::Error> {
        self.functions
            .iter_mut()
            .try_for_each(|func| func.visit(visitor, &self.scope))
    }
}

impl<'s, Ty, Scope> Node<Ty, Scope> for Document<'s, Ty, Scope> {
    type SelfWithUpgrade<NewTy, NewScope> = Document<'s, NewTy, NewScope>;
    fn upgrade<
        NewTy,
        NewScope,
        E,
        TypeUpgrader: FnMut(Ty) -> Result<NewTy, E>,
        ScopeUpgrader: FnMut(Scope) -> Result<NewScope, E>,
    >(
        self,
        type_upgrader: &mut TypeUpgrader,
        scope_upgrader: &mut ScopeUpgrader,
    ) -> Result<Self::SelfWithUpgrade<NewTy, NewScope>, E> {
        Ok(Document {
            scope: scope_upgrader(self.scope)?,
            functions: self
                .functions
                .into_iter()
                .map(|func| func.upgrade(type_upgrader, scope_upgrader))
                .collect::<Result<_, _>>()?,
        })
    }

    fn inspect_types<E, F: Fn(&Ty) -> Result<(), E>>(&self, inspector: &F) -> Result<(), E> {
        self.functions
            .iter()
            .try_for_each(|func| func.inspect_types(inspector))
    }
}

impl<Ty> DirectlyTypedNode<Ty> for Number<Ty> {
    fn r#type(&self) -> &Ty {
        &self.r#type
    }
}

impl<'s, Ty, Scope> DirectlyTypedNode<Ty> for Function<'s, Ty, Scope> {
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
