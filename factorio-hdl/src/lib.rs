pub mod ast;
mod interp;
mod lowering;
pub mod model;
mod parse;

pub use rustc_hash::FxHashMap as HashMap;
pub use rustc_hash::FxHashSet as HashSet;

pub use interp::Interpreter;
pub use lowering::lower;
pub use parse::fhdl::modules as parse;

#[cfg(test)]
mod test {
    use anyhow::Result;

    use crate::ast::Strings;
    use crate::model::{Network, Signal};
    use crate::*;

    #[test]
    fn test_factorial() -> Result<()> {
        let code = std::fs::read_to_string("examples/factorial.fhdl").unwrap();
        let mut ctx = Strings::default();
        let modules = parse(&code, &mut ctx)?;

        let mut combinators = Vec::new();
        let net = Network(0);
        let net_ids = vec![net];
        let mut max_net_id = 0;
        let signal = Signal(0);
        let signal_tick = Signal(1);
        let mut signals = HashMap::default();
        signals.insert(ctx.intern("x"), signal_tick);
        signals.insert(ctx.intern("fact_x"), signal);

        lower(
            &code,
            &modules,
            ctx.intern("factorial"),
            vec![],
            net_ids,
            signals,
            &ctx,
            &mut combinators,
            &mut max_net_id,
        )?;

        let mut interp = Interpreter::new(combinators, max_net_id);

        interp.step();
        for i in 0..13 {
            interp.step();
            assert_eq!(i, interp.state[(net, signal_tick)]);
            assert_eq!(
                (1..=i).fold(1, std::ops::Mul::mul),
                interp.state[(net, signal)]
            );
        }

        Ok(())
    }

    #[test]
    fn test_registers() -> Result<()> {
        let code = std::fs::read_to_string("examples/registers.fhdl").unwrap();
        let mut ctx = Strings::default();
        let modules = parse(&code, &mut ctx)?;

        let mut combinators = Vec::new();
        let net_ids = vec![Network(0)];
        let mut max_net_id = 0;
        let mut signals = HashMap::default();
        signals.insert(ctx.intern("write_addr"), Signal(0));
        signals.insert(ctx.intern("write_value"), Signal(1));
        signals.insert(ctx.intern("write_trigger"), Signal(2));
        signals.insert(ctx.intern("read_1_addr"), Signal(3));
        signals.insert(ctx.intern("read_1_value"), Signal(4));
        signals.insert(ctx.intern("read_2_addr"), Signal(5));
        signals.insert(ctx.intern("read_2_value"), Signal(6));

        lower(
            &code,
            &modules,
            ctx.intern("registers"),
            vec![],
            net_ids,
            signals,
            &ctx,
            &mut combinators,
            &mut max_net_id,
        )?;

        let mut interp = Interpreter::new(combinators, max_net_id);

        for _ in 0..1000 {
            interp.step();
            // todo: check if working as expected
        }

        Ok(())
    }
}
