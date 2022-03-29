mod ast;
mod interp;
mod lowering;
mod model;
mod parse;
// Add just in time compilation just for fun? (requires restriction on logical signals first)

pub use rustc_hash::FxHashMap as HashMap;

use anyhow::Result;
use model::Signal;

fn main() -> Result<()> {
    let code = std::fs::read_to_string("test.fhdl").unwrap();
    let mut ctx = ast::Strings::default();
    let module = &parse::fhdl::modules(&code, &mut ctx)?[0];

    let mut combinators = Vec::new();
    let mut net_ids = HashMap::default();
    net_ids.insert(ctx.intern("io"), model::Network(0));
    let mut signals = HashMap::default();
    signals.insert(ctx.intern("write_addr"), Signal(0));
    signals.insert(ctx.intern("write_value"), Signal(1));
    signals.insert(ctx.intern("write_trigger"), Signal(2));
    signals.insert(ctx.intern("read_1_addr"), Signal(3));
    signals.insert(ctx.intern("read_1_value"), Signal(4));
    signals.insert(ctx.intern("read_2_addr"), Signal(5));
    signals.insert(ctx.intern("read_2_value"), Signal(6));
    let mut max_net_id = 6;

    lowering::lower(
        &module,
        vec![],
        net_ids,
        signals,
        &ctx,
        &code,
        &mut combinators,
        &mut max_net_id,
    )?;

    let mut interp = interp::Interpreter::new(combinators, max_net_id);

    for _ in 0..1000 {
        interp.step();
    }

    Ok(())
}
