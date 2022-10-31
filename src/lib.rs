use core::fmt;
pub use std::collections::BTreeMap as Map;

// syntax.scala

pub type Pgrm = Vec<(bool, String, Term)>;

#[derive(Clone, Debug)]
pub enum Term {
    Lit {
        value: i64,
    },
    Var {
        name: String,
    },
    Lam {
        name: String,
        rhs: Box<Term>,
    },
    App {
        lhs: Box<Term>,
        rhs: Box<Term>,
    },
    Rcd(Map<String, Term>),
    Sel {
        receiver: Box<Term>,
        field: String,
    },
    Let {
        is_rec: bool,
        name: String,
        rhs: Box<Term>,
        body: Box<Term>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeVariable {
    pub name_hint: String,
    pub hash: u64,
}

#[derive(Clone, Debug)]
pub enum Type {
    Top,
    Bot,
    Union(Box<Type>, Box<Type>),
    Inter(Box<Type>, Box<Type>),
    Function(Box<Type>, Box<Type>),
    Record(Map<String, Type>),
    Recursive { uv: TypeVariable, body: Box<Type> },
    Primitive(&'static str),
    TVariable(TypeVariable),
}

// helpers.scala

fn parens_if(
    f: &mut fmt::Formatter<'_>,
    mut s: impl FnMut(&mut fmt::Formatter<'_>) -> fmt::Result,
    cnd: bool,
) -> fmt::Result {
    if cnd {
        f.write_str("(")?;
        s(f)?;
        f.write_str(")")
    } else {
        s(f)
    }
}

impl Type {
    pub fn children(&self) -> Vec<&'_ Type> {
        match self {
            Type::Top | Type::Bot | Type::TVariable(_) | Type::Primitive(_) => Vec::new(),
            Type::Union(l, r) | Type::Inter(l, r) | Type::Function(l, r) => vec![l, r],
            Type::Recursive { body, .. } => vec![body],
            Type::Record(m) => m.values().map(|i| &*i).collect(),
        }
    }

    pub fn type_vars(&self) -> Vec<&'_ TypeVariable> {
        use core::iter::once;
        match self {
            Type::TVariable(uv) => vec![uv],
            Type::Recursive { uv, body } => once(uv).chain(body.type_vars()).collect(),
            _ => self
                .children()
                .into_iter()
                .flat_map(Type::type_vars)
                .collect(),
        }
    }

    fn show_in(
        &self,
        f: &mut fmt::Formatter<'_>,
        ctx: &Map<TypeVariable, String>,
        outer_prec: u32,
    ) -> fmt::Result {
        match self {
            Type::Top => f.write_str("⊤"),
            Type::Bot => f.write_str("⊥"),
            Type::Primitive(name) => f.write_str(name),
            Type::TVariable(uv) => f.write_str(&ctx[uv]),
            Type::Recursive { uv, body } => {
                body.show_in(f, ctx, 31)?;
                f.write_str(" as ")?;
                f.write_str(&ctx[uv])
            }
            Type::Function(l, r) => parens_if(
                f,
                |f| {
                    l.show_in(f, ctx, 11)?;
                    f.write_str(" -> ")?;
                    r.show_in(f, ctx, 10)
                },
                outer_prec > 10,
            ),
            Type::Union(l, r) => parens_if(
                f,
                |f| {
                    l.show_in(f, ctx, 20)?;
                    f.write_str(" ∨ ")?;
                    r.show_in(f, ctx, 20)
                },
                outer_prec > 20,
            ),
            Type::Inter(l, r) => parens_if(
                f,
                |f| {
                    l.show_in(f, ctx, 25)?;
                    f.write_str(" ∧ ")?;
                    r.show_in(f, ctx, 25)
                },
                outer_prec > 25,
            ),
            Type::Record(m) => {
                let mut dm = f.debug_map();
                for (k, v) in m {
                    dm.entry(&k, &v.to_string());
                }
                dm.finish()
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tvs = self.type_vars();
        tvs.sort_unstable();
        tvs.dedup();
        assert!(
            tvs.len() < (b'z' - b'a').into(),
            "TODO handle case of not enough chars"
        );
        let ctx: Map<TypeVariable, String> = tvs
            .into_iter()
            .enumerate()
            .map(|(idx, i)| {
                (
                    i.clone(),
                    char::from_u32((b'a' as usize + idx).try_into().unwrap())
                        .unwrap()
                        .to_string(),
                )
            })
            .collect();
        self.show_in(f, &ctx, 0)
    }
}
