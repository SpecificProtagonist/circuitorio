mod model;
mod parse;

fn main() {
    let code = std::fs::read_to_string("syntax.fhdl").unwrap();
    let mut ctx = parse::Ctx::default();
    let module = parse::fhdl::modules(&code, &mut ctx);
    match module {
        Ok(module) => println!("{:?}\n\n{:?}", ctx.idents, module),
        Err(err) => println!("{}", err),
    }
}
