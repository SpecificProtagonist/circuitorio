use std::ops::Index;

use crate::model::{
    AbstractSignal, ArithOp, Combinator, Connector, DecideOp, Network, Signal, SignalOrConst,
};
use crate::HashMap;

pub struct State(Vec<HashMap<Signal, i32>>);

impl State {
    pub fn clear(&mut self) {
        for net in &mut self.0 {
            net.clear()
        }
    }

    #[inline]
    pub fn add(&mut self, net: Network, signal: Signal, value: i32) {
        let slot = self.0[net.0 as usize].entry(signal).or_default();
        *slot = slot.wrapping_add(value)
    }
}

impl Index<Network> for State {
    type Output = HashMap<Signal, i32>;

    fn index(&self, index: Network) -> &Self::Output {
        &self.0[index.0 as usize]
    }
}

impl Index<(Network, Signal)> for State {
    type Output = i32;

    fn index(&self, index: (Network, Signal)) -> &Self::Output {
        static ZERO: i32 = 0;
        self.0[index.0 .0 as usize].get(&index.1).unwrap_or(&ZERO)
    }
}

pub struct Interpreter {
    pub model: Vec<Combinator>,
    pub state: State,
    back_buffer: State,
}

impl Interpreter {
    pub fn new(model: Vec<Combinator>, max_net: u32) -> Self {
        Self {
            model,
            state: State(vec![Default::default(); max_net as usize + 1]),
            back_buffer: State(vec![Default::default(); max_net as usize + 1]),
        }
    }

    pub fn step(&mut self) {
        self.back_buffer.clear();
        for combinator in &self.model {
            step(combinator, &self.state, &mut self.back_buffer)
        }
        std::mem::swap(&mut self.state, &mut self.back_buffer)
    }
}

fn step(comb: &Combinator, old: &State, new: &mut State) {
    match comb {
        Combinator::Constant { output, values } => {
            for (signal, value) in values {
                add(*output, new, *signal, *value)
            }
        }
        Combinator::Arithmetic {
            input,
            output,
            left,
            right,
            out,
            op,
        } => {
            let right = match right {
                SignalOrConst::Signal(signal) => get(*input, old, *signal),
                SignalOrConst::ConstValue(value) => *value,
            };
            match left {
                AbstractSignal::Signal(signal) => {
                    let value = calc(get(*input, old, *signal), right, *op);
                    match out {
                        AbstractSignal::Signal(signal) => add(*output, new, *signal, value),
                        _ => unreachable!(),
                    }
                }
                AbstractSignal::Each => match out {
                    AbstractSignal::Signal(signal) => {
                        let mut acc = 0i32;
                        for (_, value) in get_all(*input, old) {
                            acc = acc.wrapping_add(calc(value, right, *op));
                        }
                        add(*output, new, *signal, acc)
                    }
                    AbstractSignal::Each => {
                        for (signal, value) in get_all(*input, old) {
                            add(*output, new, signal, calc(value, right, *op));
                        }
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }
        Combinator::Decider {
            input,
            output,
            left,
            right,
            out,
            op,
            output_one,
        } => {
            let right = match right {
                SignalOrConst::Signal(signal) => get(*input, old, *signal),
                SignalOrConst::ConstValue(value) => *value,
            };
            let passes = |left: i32| match op {
                DecideOp::Greater => left > right,
                DecideOp::Less => left < right,
                DecideOp::GreaterOrEqual => left >= right,
                DecideOp::LessOrEqual => left <= right,
                DecideOp::Equal => left == right,
                DecideOp::NotEqual => left != right,
            };
            match left {
                AbstractSignal::Signal(left) => {
                    if passes(get(*input, old, *left)) {
                        match out {
                            AbstractSignal::Signal(signal) => add(
                                *output,
                                new,
                                *signal,
                                if *output_one {
                                    1
                                } else {
                                    get(*input, old, *signal)
                                },
                            ),
                            AbstractSignal::Every => {
                                for (signal, value) in get_all(*input, old) {
                                    add(*output, new, signal, if *output_one { 1 } else { value })
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                AbstractSignal::Each => match out {
                    AbstractSignal::Signal(signal) => {
                        let mut acc = 0i32;
                        for (_, value) in get_all(*input, old) {
                            if passes(value) {
                                acc = acc.wrapping_add(if *output_one { 1 } else { value });
                            }
                        }
                        add(*output, new, *signal, acc)
                    }
                    AbstractSignal::Each => {
                        for (signal, value) in get_all(*input, old) {
                            if passes(value) {
                                add(*output, new, signal, if *output_one { 1 } else { value });
                            }
                        }
                    }
                    _ => unreachable!(),
                },
                AbstractSignal::Every | AbstractSignal::Any => {
                    let input = get_all(*input, old);
                    if match left {
                        AbstractSignal::Any => input.values().copied().any(passes),
                        AbstractSignal::Every => input.values().copied().all(passes),
                        _ => unreachable!(),
                    } {
                        match out {
                            AbstractSignal::Signal(signal) => add(
                                *output,
                                new,
                                *signal,
                                if *output_one { 1 } else { input[signal] },
                            ),
                            AbstractSignal::Every => {
                                for (signal, value) in input {
                                    add(*output, new, signal, if *output_one { 1 } else { value })
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
        }
    }
}

#[inline]
fn add(output: Connector, state: &mut State, signal: Signal, value: i32) {
    if let Some(net) = output.red {
        state.add(net, signal, value)
    }
    if let Some(net) = output.green {
        state.add(net, signal, value)
    }
}

#[inline]
fn get(input: Connector, state: &State, signal: Signal) -> i32 {
    let mut value = 0;
    if let Some(net) = input.red {
        value = state[(net, signal)]
    }
    if let Some(net) = input.green {
        value = value.wrapping_add(state[(net, signal)])
    }
    value
}

fn get_all(input: Connector, state: &State) -> HashMap<Signal, i32> {
    let mut values = HashMap::default();
    if let Some(net) = input.red {
        for (&signal, &value) in &state[net] {
            if value != 0 {
                values.insert(signal, value);
            }
        }
    }
    if let Some(net) = input.green {
        for (&signal, &value) in &state[net] {
            if value != 0 {
                let slot = values.entry(signal).or_default();
                *slot = slot.wrapping_add(value)
            }
        }
    }
    values
}

pub fn calc(left: i32, right: i32, op: ArithOp) -> i32 {
    match op {
        ArithOp::Add => left.wrapping_add(right),
        ArithOp::Sub => left.wrapping_sub(right),
        ArithOp::Mul => left.wrapping_mul(right),
        ArithOp::Div => {
            if right == 0 {
                0
            } else {
                left.wrapping_div(right)
            }
        }
        ArithOp::Mod => {
            if right == 0 {
                0
            } else {
                left % right
            }
        }
        ArithOp::Exp => left.wrapping_pow(right as u32),
        ArithOp::ShiftLeft => left.wrapping_shl(right as u32),
        ArithOp::ShiftRight => left.wrapping_shr(right as u32),
        ArithOp::And => left & right,
        ArithOp::Or => left | right,
        ArithOp::Xor => left ^ right,
    }
}
