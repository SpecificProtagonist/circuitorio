#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct Signal(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Color {
    Red,
    Green,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Network(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Connector {
    pub red: Option<Network>,
    pub green: Option<Network>,
}

#[derive(Copy, Clone, Debug)]
pub enum SignalOrConst {
    Signal(Signal),
    ConstValue(i32),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum AbstractSignal {
    Signal(Signal),
    Any,
    Every,
    Each,
}

#[derive(Clone, Debug)]
pub enum Combinator {
    Constant {
        output: Connector,
        values: Vec<(Signal, i32)>,
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
