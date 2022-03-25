// TODO: faster hasher
use std::collections::HashMap;
pub use Color::*;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Ident(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Color {
    Red,
    Green,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct NetworkId(u32);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Network {
    pub color: Color,
    pub signals: Vec<Ident>,
}

// During parsing, no info about the networks is available.
// Colors will be wrong untill they get fixed up.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Connector {
    pub red: Option<Ident>,
    pub green: Option<Ident>,
}

// TODO: 1.1.13 Decider Anything signal

#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    Xor,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum DecideOp {
    Greater,
    Less,
    Equal,
    GreaterOrEqual,
    LessOrEqual,
    NotEqual,
}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub args: Vec<Ident>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Block {
    pub networks: HashMap<Ident, Network>,
    pub param_decls: HashMap<Ident, Expr>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Block(Block),
    Combinator(Combinator),
    Instance(Instance),
    Loop(Loop),
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
    pub body: Block,
}

#[derive(Debug)]
pub struct ArgNet {
    pub name: Ident,
    /// arg name to local name
    pub signal_map: HashMap<Ident, Ident>,
}

#[derive(Copy, Clone, Debug)]
pub enum Expr {
    Literal(i32),
    Param(Ident),
}
