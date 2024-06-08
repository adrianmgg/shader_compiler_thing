//! type stuff
//!
//! based on
//! https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=a15a24394c145e4e02afa3b48bb51ea1
//! (via https://github.com/Kixiron/rust-langdev?tab=readme-ov-file#type-checking-1)

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Integer {
    pub(crate) width: u32,
    pub(crate) signed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Float {
    pub(crate) width: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct Vector<Ty> {
    pub(crate) component_type: Ty,
    pub(crate) component_count: u32,
}

#[derive(Debug, Clone)]
pub(crate) struct Function<Ty, Tyv> {
    pub(crate) return_type: Ty,
    pub(crate) parameter_types: Vec<Tyv>,
}

// ================================================================================

/// concrete type
pub(crate) enum Type {
    Boolean,
    Integer(Integer),
    Float(Float),
    Vector(Vector<Box<Type>>),
    Function(Function<Box<Type>, Type>),
}

// ================================================================================

/// type term identifier
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) struct TypeId(usize);

/// type term info
#[derive(Debug, Clone)]
pub(crate) enum TypeInfo {
    /// no info known yet
    Unknown,
    /// same as another type term
    Ref(TypeId),
    Boolean,
    Integer(Integer),
    Float(Float),
    Vector(Vector<TypeId>),
    Function(Function<TypeId, TypeId>),
}

pub(crate) struct Engine {
    vars: Vec<TypeInfo>,
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum EngineError {
    #[error("Type mismatch between {a:?} and {b:?}")]
    UnifyTypeMismatch { a: TypeId, b: TypeId },
    #[error("invalid typeid {0:?}")]
    InvalidTypeId(TypeId),
    #[error("cannot infer")]
    CannotInfer,
}

impl Engine {
    pub(crate) fn new() -> Self {
        Self { vars: Vec::new() }
    }

    pub(crate) fn insert(&mut self, info: TypeInfo) -> TypeId {
        let id = TypeId(self.vars.len());
        self.vars.push(info);
        id
    }

    fn getvar(&self, id: TypeId) -> Result<&TypeInfo, EngineError> {
        self.vars.get(id.0).ok_or(EngineError::InvalidTypeId(id))
    }

    fn getvar_mut(&mut self, id: TypeId) -> Result<&mut TypeInfo, EngineError> {
        self.vars
            .get_mut(id.0)
            .ok_or(EngineError::InvalidTypeId(id))
    }

    fn replace(&mut self, id: TypeId, info: TypeInfo) -> Result<(), EngineError> {
        *(self.getvar_mut(id)?) = info;
        Ok(())
    }

    pub(crate) fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), EngineError> {
        match (self.getvar(a).cloned()?, self.getvar(b).cloned()?) {
            // follow refs
            (TypeInfo::Ref(a_), _) => self.unify(a_, b),
            (_, TypeInfo::Ref(b_)) => self.unify(a, b_),

            (TypeInfo::Unknown, _) => self.replace(a, TypeInfo::Ref(b)),
            (_, TypeInfo::Unknown) => self.replace(b, TypeInfo::Ref(a)),

            (TypeInfo::Boolean, TypeInfo::Boolean) => Ok(()),

            (TypeInfo::Integer(i_a), TypeInfo::Integer(i_b)) if i_a == i_b => Ok(()),
            (TypeInfo::Float(f_a), TypeInfo::Float(f_b)) if f_a == f_b => Ok(()),

            (TypeInfo::Function(func_a), TypeInfo::Function(func_b)) => {
                if func_a.parameter_types.len() != func_b.parameter_types.len() {
                    Err(EngineError::UnifyTypeMismatch { a, b })
                } else {
                    self.unify(func_a.return_type, func_b.return_type)?;
                    for (x, y) in
                        std::iter::zip(func_a.parameter_types.iter(), func_b.parameter_types.iter())
                    {
                        self.unify(*x, *y)?;
                    }
                    Ok(())
                }
            }

            (TypeInfo::Vector(vec_a), TypeInfo::Vector(vec_b)) => {
                if vec_a.component_count != vec_b.component_count {
                    Err(EngineError::UnifyTypeMismatch { a, b })
                } else {
                    self.unify(vec_a.component_type, vec_b.component_type)
                }
            }

            (_, _) => Err(EngineError::UnifyTypeMismatch { a, b }),
        }
    }

    pub(crate) fn reconstruct(&self, id: TypeId) -> Result<Type, EngineError> {
        match self.getvar(id)? {
            TypeInfo::Unknown => Err(EngineError::CannotInfer),
            TypeInfo::Ref(id) => self.reconstruct(*id),
            TypeInfo::Boolean => Ok(Type::Boolean),
            TypeInfo::Integer(i) => Ok(Type::Integer(*i)),
            TypeInfo::Float(f) => Ok(Type::Float(*f)),
            TypeInfo::Function(func) => Ok(Type::Function(Function {
                return_type: Box::new(self.reconstruct(func.return_type)?),
                parameter_types: func
                    .parameter_types
                    .iter()
                    .map(|t| self.reconstruct(*t))
                    .collect::<Result<_, _>>()?,
            })),
            TypeInfo::Vector(vec) => Ok(Type::Vector(Vector {
                component_count: vec.component_count,
                component_type: Box::new(self.reconstruct(vec.component_type)?),
            })),
        }
    }
}
