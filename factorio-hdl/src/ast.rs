use crate::model::{ArithOp, DecideOp};
// TODO: faster hasher
pub use crate::model::Color;
use crate::HashMap;
use std::ops::Index;

#[derive(Default)]
pub struct Strings {
    pub ident_ids: HashMap<String, Ident>,
    pub idents: Vec<String>,
}

impl Strings {
    pub fn intern(&mut self, str: &str) -> Ident {
        if let Some(&id) = self.ident_ids.get(str) {
            id
        } else {
            let id = Ident(self.ident_ids.len() as u32);
            self.ident_ids.insert(str.into(), id);
            self.idents.push(str.into());
            id
        }
    }
}

impl Index<Ident> for Strings {
    type Output = str;

    fn index(&self, index: Ident) -> &Self::Output {
        &self.idents[index.0 as usize]
    }
}

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Ident(pub u32);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Network {
    pub name: Ident,
    pub color: Color,
    pub signals: Vec<Ident>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Connector(pub Ident, pub Option<Ident>);

// TODO: 1.1.13 Decider Anything signal

/// As the parser can't differentiate between signal and param names,
/// a simple param will be represented as Self::Signal
#[derive(Clone, Debug)]
pub enum SignalOrConst {
    Signal(Ident),
    ConstValue(Expr),
}

// TODO: AbstractSignalOrConst
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum AbstractSignal {
    Signal(Ident),
    Any,
    Every,
    Each,
}

#[derive(Clone, Debug)]
pub enum Combinator {
    Constant {
        output: Connector,
        out: Ident,
        value: Expr,
    },
    Arithmetic {
        input: Connector,
        output: Connector,
        left: AbstractSignal,
        right: SignalOrConst,
        out: AbstractSignal,
        op: ArithOp,
    },
    Decider {
        input: Connector,
        output: Connector,
        left: AbstractSignal,
        right: SignalOrConst,
        out: AbstractSignal,
        op: DecideOp,
        output_one: bool,
    },
}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub args: HashMap<Ident, Network>,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct Statement {
    pub start_pos: usize,
    pub inner: StatementInner,
}

#[derive(Debug)]
pub enum StatementInner {
    Block(Vec<Statement>),
    Combinator(Combinator),
    Instance(Instance),
    Loop(Loop),
    NetDecl(Network),
    ParamDecl(Ident, Expr),
}

#[derive(Debug)]
pub struct Instance {
    pub name: Ident,
    pub params: Vec<Expr>,
    pub args: Vec<ArgNet>,
}

#[derive(Debug)]
pub struct Loop {
    pub iter: Ident,
    pub min: Expr,
    pub max: Expr,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub struct ArgNet {
    pub name: Ident,
    /// arg name to local name
    pub signal_map: HashMap<Ident, Ident>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(i32),
    Param(Ident),
    Calc(Box<Expr>, Box<Expr>, ArithOp),
}
