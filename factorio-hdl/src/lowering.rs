use crate::HashMap;
use anyhow::{anyhow, bail, Context, Result};

use crate::ast::{self, Expr, IOClass, Ident, Loop, Statement, StatementInner, Strings};
use crate::interp;
use crate::model::{self, Color, Signal};

const MAX_SIGNALS_PER_CONST_COMBINATOR: usize = 15;

pub fn lower(
    module: &ast::Module,
    params: Vec<i32>,
    net_ids: HashMap<Ident, model::Network>,
    signal_ids: HashMap<Ident, model::Signal>,
    ctx: &Strings,
    code: &str,
    combinators: &mut Vec<model::Combinator>,
    max_net_id: &mut u32,
) -> Result<()> {
    // kept sorted to allow finding the lowest free signal
    let mut signals: Vec<Signal> = signal_ids.values().copied().collect();
    signals.sort_unstable();
    let mut constants = HashMap::default();
    let params = {
        let mut map = HashMap::default();
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
            bail!(
                "ambiguity between param and signal {} in module definition {}",
                &ctx[*name],
                &ctx[module.name]
            )
        }
    }

    lower_block(
        &module.body,
        code,
        ctx,
        combinators,
        &mut constants,
        params,
        net_ids,
        module.args.clone(),
        signal_ids,
        signals,
        max_net_id,
    )?;

    for (output, values) in constants {
        combinators.push(model::Combinator::Constant {
            output,
            values: values.into_iter().collect(),
        })
    }

    Ok(())
}

fn lower_block(
    statements: &[Statement],
    code: &str,
    ctx: &Strings,
    combinators: &mut Vec<model::Combinator>,
    constants: &mut HashMap<model::Connector, HashMap<Signal, i32>>,
    mut params: HashMap<Ident, i32>,
    mut net_ids: HashMap<Ident, model::Network>,
    mut networks: HashMap<Ident, ast::Network>,
    mut signal_ids: HashMap<Ident, model::Signal>,
    mut signals: Vec<Signal>,
    max_net_id: &mut u32,
) -> Result<()> {
    for stm in statements {
        lower_statement(
            stm,
            code,
            ctx,
            combinators,
            constants,
            &mut params,
            &mut net_ids,
            &mut networks,
            &mut signal_ids,
            &mut signals,
            max_net_id,
        )
        .with_context(|| {
            format!(
                "line {}",
                code[0..stm.start_pos]
                    .chars()
                    .filter(|&c| c == '\n')
                    .count()
                    + 1
            )
        })?
    }

    Ok(())
}

fn lower_statement(
    stm: &Statement,
    code: &str,
    ctx: &Strings,
    combinators: &mut Vec<model::Combinator>,
    constants: &mut HashMap<model::Connector, HashMap<Signal, i32>>,
    params: &mut HashMap<Ident, i32>,
    net_ids: &mut HashMap<Ident, model::Network>,
    networks: &mut HashMap<Ident, ast::Network>,
    signal_ids: &mut HashMap<Ident, model::Signal>,
    signals: &mut Vec<Signal>,
    max_net_id: &mut u32,
) -> Result<()> {
    match &stm.inner {
        StatementInner::Block(_) => todo!(),
        StatementInner::Instance(_) => todo!(),
        StatementInner::Loop(Loop {
            iter,
            min,
            max,
            body,
        }) => {
            for value in eval(ctx, params, min)?..eval(ctx, params, max)? {
                if signal_ids.contains_key(iter) | params.contains_key(iter) {
                    bail!("redefinition of {}", &ctx[*iter])
                }
                let mut params = params.clone();
                params.insert(*iter, value);
                lower_block(
                    body,
                    code,
                    ctx,
                    combinators,
                    constants,
                    params,
                    net_ids.clone(),
                    networks.clone(),
                    signal_ids.clone(),
                    signals.clone(),
                    max_net_id,
                )?;
            }
        }
        StatementInner::Combinator(combinator) => {
            // TODO: error when using any/all/each on net with unknown signals
            match combinator {
                ast::Combinator::Constant { output, out, value } => {
                    let connector = connector(ctx, &net_ids, &networks, *output)?;
                    let signal = signal(ctx, &signal_ids, &networks, *output, false, *out)?;
                    let combinator = constants.entry(connector).or_default();
                    let slot = combinator.entry(signal).or_default();
                    let value = eval(ctx, &params, &value)?;
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
                    let connector_in = connector(ctx, &net_ids, &networks, *input)?;
                    let connector_out = connector(ctx, &net_ids, &networks, *output)?;
                    let left =
                        match left {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, *input, true, *name)?,
                            ),
                            ast::AbstractSignal::Each => model::AbstractSignal::Each,
                            _ => unreachable!(),
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
                                    *input,
                                    true,
                                    *name,
                                )?)
                            }
                        }
                        ast::SignalOrConst::ConstValue(value) => {
                            model::SignalOrConst::ConstValue(eval(ctx, &params, &value)?)
                        }
                    };
                    let out =
                        match out {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, *output, false, *name)?,
                            ),
                            ast::AbstractSignal::Each => {
                                if left == model::AbstractSignal::Each {
                                    model::AbstractSignal::Each
                                } else {
                                    bail!("'each' output requires 'each' input")
                                }
                            }
                            _ => unreachable!(),
                        };

                    combinators.push(model::Combinator::Arithmetic {
                        input: connector_in,
                        output: connector_out,
                        left,
                        right,
                        out,
                        op: *op,
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
                    let connector_in = connector(ctx, &net_ids, &networks, *input)?;
                    let connector_out = connector(ctx, &net_ids, &networks, *output)?;
                    let left =
                        match left {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, *input, true, *name)?,
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
                                    *input,
                                    true,
                                    *name,
                                )?)
                            }
                        }
                        ast::SignalOrConst::ConstValue(value) => {
                            model::SignalOrConst::ConstValue(eval(ctx, &params, &value)?)
                        }
                    };
                    let out =
                        match out {
                            ast::AbstractSignal::Signal(name) => model::AbstractSignal::Signal(
                                signal(ctx, &signal_ids, &networks, *output, false, *name)?,
                            ),
                            ast::AbstractSignal::Any => {
                                bail!("'any' is not yet implemented for output")
                            }
                            ast::AbstractSignal::Every => match left {
                                model::AbstractSignal::Each => {
                                    bail!("'every' output incompatible with 'each' input")
                                }
                                _ => model::AbstractSignal::Every,
                            },
                            ast::AbstractSignal::Each => match left {
                                model::AbstractSignal::Each => model::AbstractSignal::Each,
                                _ => bail!("'each' output requires 'each' input"),
                            },
                        };
                    combinators.push(model::Combinator::Decider {
                        input: connector_in,
                        output: connector_out,
                        left,
                        right,
                        out,
                        op: *op,
                        output_one: *output_one,
                    });
                }
            }
        }
        StatementInner::NetDecl(net) => {
            if networks.contains_key(&net.name) {
                bail!("redefinition of network {}", &ctx[net.name])
            }
            *max_net_id += 1;
            let id = model::Network(*max_net_id);
            net_ids.insert(net.name, id);
            networks.insert(net.name, net.clone());
            for (signal, _) in &net.signals {
                if params.contains_key(signal) {
                    bail!("ambiguity between param and signal {}", &ctx[*signal])
                }
                if !signal_ids.contains_key(signal) {
                    // Add the signal at the lowest free id, as the amount of signals in factorio is limited
                    let mut id = 0;
                    for signal in signals.iter() {
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
            if signal_ids.contains_key(name) | params.contains_key(name) {
                bail!("redefinition of {}", &ctx[*name])
            }
            params.insert(*name, eval(ctx, &params, value)?);
        }
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
    is_input: bool,
    name: Ident,
) -> Result<Signal> {
    if !networks[&connector.0].signals.contains_key(&name) {
        if let Some(net) = connector.1 {
            if !networks[&net].signals.contains_key(&name) {
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

    let io_class_valid = |class: Option<&IOClass>| match class {
        Some(IOClass::In) => is_input,
        Some(IOClass::Out) => !is_input,
        _ => true,
    };
    if !io_class_valid(networks[&connector.0].signals.get(&name))
        | !io_class_valid(
            connector
                .1
                .map(|n| networks[&n].signals.get(&name))
                .flatten(),
        )
    {
        bail!(
            "tried to {} signal with mismatching io class",
            if is_input { "read" } else { "write" }
        )
    }

    signals
        .get(&name)
        .copied()
        .ok_or_else(|| anyhow!("undefined signal {}", &ctx[name]))
}

fn eval(ctx: &Strings, params: &HashMap<Ident, i32>, expr: &Expr) -> Result<i32> {
    Ok(match expr {
        Expr::Literal(value) => *value,
        Expr::Param(name) => params
            .get(&name)
            .copied()
            .ok_or_else(|| anyhow!("undefined param {}", &ctx[*name]))?,
        Expr::Calc(x, y, op) => interp::calc(eval(ctx, params, &x)?, eval(ctx, params, &y)?, *op),
    })
}
