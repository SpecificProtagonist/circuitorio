use anyhow::{anyhow, bail, Context, Result};
use std::collections::HashMap;

use crate::ast::{self, Expr, Ident, StatementInner, Strings};
use crate::model::{self, Color, Signal};

const MAX_SIGNALS_PER_CONST_COMBINATOR: usize = 15;

pub fn lower(
    module: &ast::Module,
    params: Vec<i32>,
    mut net_ids: HashMap<Ident, model::Network>,
    mut signal_ids: HashMap<Ident, model::Signal>,
    ctx: &Strings,
    code: &str,
    combinators: &mut Vec<model::Combinator>,
    max_net_id: &mut u32,
) -> Result<()> {
    // trying this without blocks first

    let mut networks = module.args.clone();
    // sorted
    let mut signals: Vec<Signal> = signal_ids.values().copied().collect();
    signals.sort_unstable();
    let mut constants = HashMap::<model::Connector, HashMap<Signal, i32>>::new();
    let mut params = {
        let mut map = HashMap::new();
        if params.len() != module.params.len() {
            bail!(
                "expected {} params, got {}",
                module.params.len(),
                params.len()
            )
        }
        for i in 0..params.len() {
            map.insert(module.params[i], params[i]);
        }
        map
    };

    for name in &module.params {
        if signal_ids.contains_key(name) {
            bail!("ambiguity between param and signal {}", &ctx[*name])
        }
    }

    for stm in &module.body {
        let line: Result<()> = try {
            match &stm.inner {
                StatementInner::Block(_) => todo!(),
                StatementInner::Instance(_) => todo!(),
                StatementInner::Loop(_) => todo!(),
                StatementInner::Combinator(combinator) => match *combinator {
                    ast::Combinator::Constant { output, out, value } => {
                        let connector = connector(ctx, &net_ids, &networks, output)?;
                        let signal = signal(ctx, &signal_ids, &networks, output, out)?;
                        let combinator = constants.entry(connector).or_default();
                        let slot = combinator.entry(signal).or_default();
                        let value = eval(ctx, &params, value)?;
                        *slot = slot.wrapping_add(value);
                        if combinator.len() > MAX_SIGNALS_PER_CONST_COMBINATOR {
                            todo!()
                        }
                    }
                    ast::Combinator::Arithmetic {
                        input,
                        output,
                        left,
                        right,
                        out,
                        op,
                    } => {
                        let connector_in = connector(ctx, &net_ids, &networks, input)?;
                        let connector_out = connector(ctx, &net_ids, &networks, output)?;
                        let left = match left {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, input, name)?,
                            ),
                            ast::AbstractSignal::Any => model::AbstractSignal::Any,
                            ast::AbstractSignal::Every => model::AbstractSignal::Every,
                            ast::AbstractSignal::Each => model::AbstractSignal::Each,
                        };
                        let right = match right {
                            ast::SignalOrConst::Signal(name) => {
                                if let Some(value) = params.get(&name) {
                                    model::SignalOrConst::ConstValue(*value)
                                } else {
                                    // The parser can't differentiate between signals and simple params
                                    model::SignalOrConst::Signal(signal(
                                        ctx,
                                        &signal_ids,
                                        &networks,
                                        input,
                                        name,
                                    )?)
                                }
                            }
                            ast::SignalOrConst::ConstValue(value) => {
                                model::SignalOrConst::ConstValue(eval(ctx, &params, value)?)
                            }
                        };
                        let out = match out {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, output, name)?,
                            ),
                            ast::AbstractSignal::Any => model::AbstractSignal::Any,
                            ast::AbstractSignal::Every => model::AbstractSignal::Every,
                            ast::AbstractSignal::Each => model::AbstractSignal::Each,
                        };
                        combinators.push(model::Combinator::Arithmetic {
                            input: connector_in,
                            output: connector_out,
                            left,
                            right,
                            out,
                            op,
                        });
                    }
                    ast::Combinator::Decider {
                        input,
                        output,
                        left,
                        right,
                        out,
                        op,
                        output_one,
                    } => {
                        let connector_in = connector(ctx, &net_ids, &networks, input)?;
                        let connector_out = connector(ctx, &net_ids, &networks, output)?;
                        let left = match left {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, input, name)?,
                            ),
                            ast::AbstractSignal::Any => model::AbstractSignal::Any,
                            ast::AbstractSignal::Every => model::AbstractSignal::Every,
                            ast::AbstractSignal::Each => model::AbstractSignal::Each,
                        };
                        let right = match right {
                            ast::SignalOrConst::Signal(name) => {
                                // The parser can't differentiate between signals and simple params
                                if let Some(value) = params.get(&name) {
                                    model::SignalOrConst::ConstValue(*value)
                                } else {
                                    model::SignalOrConst::Signal(signal(
                                        ctx,
                                        &signal_ids,
                                        &networks,
                                        input,
                                        name,
                                    )?)
                                }
                            }
                            ast::SignalOrConst::ConstValue(value) => {
                                model::SignalOrConst::ConstValue(eval(ctx, &params, value)?)
                            }
                        };
                        let out = match out {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, output, name)?,
                            ),
                            ast::AbstractSignal::Any => model::AbstractSignal::Any,
                            ast::AbstractSignal::Every => model::AbstractSignal::Every,
                            ast::AbstractSignal::Each => model::AbstractSignal::Each,
                        };
                        combinators.push(model::Combinator::Decider {
                            input: connector_in,
                            output: connector_out,
                            left,
                            right,
                            out,
                            op,
                            output_one,
                        });
                    }
                },
                StatementInner::NetDecl(net) => {
                    if networks.contains_key(&net.name) {
                        bail!("redefinition of network {}", &ctx[net.name])
                    }
                    *max_net_id += 1;
                    let id = model::Network(*max_net_id);
                    net_ids.insert(net.name, id);
                    networks.insert(net.name, net.clone());
                    for signal in &net.signals {
                        if params.contains_key(signal) {
                            bail!("ambiguity between param and signal {}", &ctx[*signal])
                        }
                        if !signal_ids.contains_key(signal) {
                            // Add the signal at the lowest free id, as the amount of signals in factorio is limited
                            let mut id = 0;
                            for signal in &signals {
                                if signal.0 > id + 1 {
                                    break;
                                } else {
                                    id = signal.0
                                }
                            }
                            signal_ids.insert(*signal, Signal(id));
                            signals.push(Signal(id));
                            signals.sort_unstable();
                        }
                    }
                }
                StatementInner::ParamDecl(name, value) => {
                    if signal_ids.contains_key(name) {
                        bail!("ambiguity between param and signal {}", &ctx[*name])
                    }
                    params.insert(*name, eval(ctx, &params, *value)?);
                }
            }
        };
        line.with_context(|| {
            format!(
                "line {}",
                code[0..stm.start_pos]
                    .chars()
                    .filter(|&c| c == '\n')
                    .count()
                    + 1
            )
        })?;
    }

    for (output, values) in constants {
        combinators.push(model::Combinator::Constant {
            output,
            values: values.into_iter().collect(),
        })
    }

    Ok(())
}

fn connector(
    ctx: &Strings,
    net_ids: &HashMap<Ident, model::Network>,
    networks: &HashMap<Ident, ast::Network>,
    connector: ast::Connector,
) -> Result<model::Connector> {
    let network_1 = networks
        .get(&connector.0)
        .ok_or_else(|| anyhow!("undefined network {}", &ctx[connector.0]))?;
    let network_2 = if let Some(net) = connector.1 {
        Some(
            networks
                .get(&net)
                .ok_or_else(|| anyhow!("undefined network {}", &ctx[net]))?,
        )
    } else {
        None
    };
    match (network_1.color, network_2.map(|n| n.color)) {
        (Color::Red, None) => Ok(model::Connector {
            red: Some(net_ids[&network_1.name]),
            green: None,
        }),
        (Color::Green, None) => Ok(model::Connector {
            green: Some(net_ids[&network_1.name]),
            red: None,
        }),
        (Color::Red, Some(Color::Green)) => Ok(model::Connector {
            red: Some(net_ids[&network_1.name]),
            green: Some(net_ids[&network_2.unwrap().name]),
        }),
        (Color::Green, Some(Color::Red)) => Ok(model::Connector {
            green: Some(net_ids[&network_1.name]),
            red: Some(net_ids[&network_2.unwrap().name]),
        }),
        _ => Err(anyhow!(
            "connected networks have same color: {} and {}",
            &ctx[network_1.name],
            &ctx[network_2.unwrap().name]
        )),
    }
}

/// Assumes that connector is valid
fn signal(
    ctx: &Strings,
    signals: &HashMap<Ident, Signal>,
    networks: &HashMap<Ident, ast::Network>,
    connector: ast::Connector,
    name: Ident,
) -> Result<Signal> {
    if !networks[&connector.0].signals.contains(&name) {
        if let Some(net) = connector.1 {
            if !networks[&net].signals.contains(&name) {
                bail!(
                    "networks {} and {} do not contain signal {}",
                    &ctx[connector.0],
                    &ctx[connector.1.unwrap()],
                    &ctx[name]
                )
            }
        } else {
            bail!(
                "network {} does not contain signal {}",
                &ctx[connector.0],
                &ctx[name]
            )
        }
    }
    signals
        .get(&name)
        .copied()
        .ok_or_else(|| anyhow!("undefined signal {}", &ctx[name]))
}

fn eval(ctx: &Strings, params: &HashMap<Ident, i32>, expr: Expr) -> Result<i32> {
    match expr {
        Expr::Literal(value) => Ok(value),
        Expr::Param(name) => params
            .get(&name)
            .copied()
            .ok_or_else(|| anyhow!("undefined param {}", &ctx[name])),
    }
}
