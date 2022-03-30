use crate::HashMap;
use anyhow::{anyhow, bail, Context, Result};

use crate::ast::{self, Expr, IOClass, Ident, Loop, Module, Statement, StatementInner, Strings};
use crate::interp;
use crate::model::{self, Color, Signal};

const MAX_SIGNALS_PER_CONST_COMBINATOR: usize = 15;

pub fn lower(
    code: &str,
    modules: &HashMap<Ident, Module>,
    module: Ident,
    params: Vec<i32>,
    args: Vec<model::Network>,
    signal_ids: HashMap<Ident, model::Signal>,
    ctx: &Strings,
    combinators: &mut Vec<model::Combinator>,
    max_net_id: &mut u32,
) -> Result<()> {
    let module = modules
        .get(&module)
        .ok_or_else(|| anyhow!("undefined module {}", &ctx[module]))?;
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

    let net_ids = {
        let mut map = HashMap::default();
        if args.len() != module.args.len() {
            bail!(
                "expected {} arguments, got {}",
                module.args.len(),
                args.len()
            )
        }
        for i in 0..args.len() {
            map.insert(module.args[i].name, args[i]);
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
        modules,
        module,
        ctx,
        combinators,
        &mut constants,
        params,
        net_ids,
        module.args.iter().map(|n| (n.name, n.clone())).collect(),
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
    modules: &HashMap<Ident, Module>,
    module: &Module,
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
            modules,
            module,
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
    modules: &HashMap<Ident, Module>,
    module: &Module,
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
        StatementInner::Block(body) => {
            lower_block(
                body,
                code,
                modules,
                module,
                ctx,
                combinators,
                constants,
                params.clone(),
                net_ids.clone(),
                networks.clone(),
                signal_ids.clone(),
                signals.clone(),
                max_net_id,
            )?;
        }
        StatementInner::Instance(instance) => {
            // TODO: check if the call stack includes this module with the same parameters
            let module = modules
                .get(&instance.name)
                .ok_or_else(|| anyhow!("undefined module {}", &ctx[instance.name]))?;
            let mut instance_net_ids = Vec::new();
            let mut instance_signal_ids = HashMap::default();
            let mut signal_map = HashMap::default();
            for (arg_num, arg) in instance.args.iter().enumerate() {
                let net_local = networks
                    .get(&arg.name)
                    .ok_or_else(|| anyhow!("undefined network {}", &ctx[arg.name]))?;
                let net_arg = module
                    .args
                    .get(arg_num)
                    .ok_or_else(|| anyhow!("too many arguments"))?;

                if net_local.color != net_arg.color {
                    bail!(
                        "color mismatch for network {} (function argument {})",
                        &ctx[net_local.name],
                        &ctx[net_arg.name]
                    )
                }

                instance_net_ids.push(net_ids[&arg.name]);

                for (signal_arg, signal_local) in &arg.signal_map {
                    // Validate signal mapping
                    if !net_local.signals.contains_key(signal_local) {
                        bail!(
                            "undefined signal {} in network {}",
                            &ctx[*signal_local],
                            &ctx[net_local.name]
                        )
                    }
                    if !net_arg.signals.contains_key(signal_arg) {
                        bail!(
                            "undefined function arg signal {} in network {}",
                            &ctx[*signal_arg],
                            &ctx[net_arg.name]
                        )
                    }
                    if signal_map.get(signal_local).unwrap_or(signal_arg) != signal_arg {
                        bail!(
                            "signal {} provided for two different argument signals",
                            &ctx[*signal_local]
                        )
                    }
                    signal_map.insert(*signal_local, *signal_arg);
                    let id = *signal_ids
                        .get(signal_local)
                        .ok_or_else(|| anyhow!("undefined signal {}", &ctx[*signal_local]))?;
                    if *instance_signal_ids.get(signal_arg).unwrap_or(&id) != id {
                        bail!(
                            "two different signals provided for argument signal {}",
                            &ctx[*signal_arg]
                        )
                    }
                    if !matches!(
                        (net_local.signals[signal_local], net_arg.signals[signal_arg]),
                        (IOClass::In, IOClass::In)
                            | (IOClass::Out, IOClass::In)
                            | (IOClass::InOut, _),
                    ) {
                        bail!(
                            "io class mismatch for {}: {}",
                            &ctx[*signal_arg],
                            &ctx[*signal_local]
                        )
                    }
                    // Everything valid
                    instance_signal_ids.insert(*signal_arg, id);
                }
            }
            lower(
                code,
                modules,
                instance.name,
                instance
                    .params
                    .iter()
                    .map(|e| eval(ctx, params, e))
                    .collect::<Result<_>>()?,
                instance_net_ids,
                instance_signal_ids,
                ctx,
                combinators,
                max_net_id,
            )?;
        }
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
                    modules,
                    module,
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
            // TODO: error on using a signal from a local network together
            // with a arg network that doesn't contain the signal
            // (allow via new clobber attribute?)
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
                            ast::AbstractSignal::Each => {
                                if module.args.iter().find(|n| n.name == input.0).is_none()
                                    | input.1.map_or(false, |n| {
                                        module.args.iter().find(|net| net.name == n).is_none()
                                    })
                                {
                                    bail!("Cannot use logical signal on module argument as input")
                                }
                                model::AbstractSignal::Each
                            }
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
                                    check_logical(networks, *input, *output)?;
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
                                check_logical(networks, *input, *output)?;
                                bail!("'any' is not yet implemented for output")
                            }
                            ast::AbstractSignal::Every => match left {
                                model::AbstractSignal::Each => {
                                    bail!("'every' output incompatible with 'each' input")
                                }
                                _ => {
                                    check_logical(networks, *input, *output)?;
                                    model::AbstractSignal::Every
                                }
                            },
                            ast::AbstractSignal::Each => match left {
                                model::AbstractSignal::Each => {
                                    check_logical(networks, *input, *output)?;
                                    model::AbstractSignal::Each
                                }
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

fn check_logical(
    networks: &HashMap<Ident, ast::Network>,
    input: ast::Connector,
    output: ast::Connector,
) -> Result<()> {
    for signal in networks[&input.0]
        .signals
        .keys()
        .chain(input.1.iter().flat_map(|n| networks[n].signals.keys()))
    {
        if !networks[&output.0].signals.contains_key(signal)
            | !output
                .1
                .map_or(true, |n| networks[&n].signals.contains_key(signal))
        {
            bail!("every/each output used, but output networks do not contain all input network signals")
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
        if is_input {
            bail!("tried to read signal {} with io class out", &ctx[name])
        } else {
            bail!("tried to write signal {} with io class in", &ctx[name])
        }
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
