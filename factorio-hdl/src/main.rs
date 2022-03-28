#![feature(try_blocks)]

mod ast;
mod lowering;
mod model;
mod parse;

use std::collections::HashMap;

use anyhow::Result;
use model::Signal;

fn main() -> Result<()> {
    let code = std::fs::read_to_string("test.fhdl").unwrap();
    let mut ctx = ast::Strings::default();
    let module = &parse::fhdl::modules(&code, &mut ctx)?[0];

    let mut combinators = Vec::new();
    let mut net_ids = HashMap::new();
    net_ids.insert(ctx.intern("io"), model::Network(0));
    let mut signals = HashMap::new();
    signals.insert(ctx.intern("write_addr"), Signal(0));
    signals.insert(ctx.intern("write_value"), Signal(1));
    signals.insert(ctx.intern("write_trigger"), Signal(2));
    signals.insert(ctx.intern("read_1_addr"), Signal(3));
    signals.insert(ctx.intern("read_1_value"), Signal(4));
    signals.insert(ctx.intern("read_2_addr"), Signal(5));
    signals.insert(ctx.intern("read_2_value"), Signal(6));

    lowering::lower(
        &module,
        vec![],
        net_ids,
        signals,
        &ctx,
        &code,
        &mut combinators,
        &mut 0,
    )?;
    Ok(())
}
