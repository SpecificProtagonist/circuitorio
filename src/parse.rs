use std::collections::HashMap;

use crate::model::*;

#[derive(Default)]
pub struct Ctx {
    pub ident_ids: HashMap<String, Ident>,
    pub idents: Vec<String>,
}

impl Ctx {
    fn intern(&mut self, str: &str) -> Ident {
        if let Some(&id) = self.ident_ids.get(str) {
            id
        } else {
            let id = Ident(self.ident_ids.len() as u32);
            self.ident_ids.insert(str.into(), id);
            self.idents.push(str.into());
            id
        }
    }
}

enum StatementOrDecl {
    Statement(Statement),
    Decl(Ident, Network),
}

peg::parser! { pub grammar fhdl() for str {

    rule _() = quiet!{[' ' | '\n' | '\t']* ("#"[^'\n']*"\n"?_)?}

    pub rule modules(ctx: &mut Ctx) -> Vec<Module> = module(ctx)*

    rule module(ctx: &mut Ctx) -> Module =
        "module" _ name:ident(ctx)
        params:("<" _ items:ident(ctx)* ">" _ {items})?
        "(" _ io:net_decl(ctx)* ")" _
        body: block(ctx)
    {
        let mut body = body;
        let mut args = Vec::new();
        for (name, net) in io {
            args.push(name);
            body.networks.insert(name, net);
        }
        Module {
            name: name,
            params: params.unwrap_or_default(),
            args,
            body,
        }
    }

    rule block(ctx: &mut Ctx) -> Block = "{" _ items:statement_or_decl(ctx)* "}" _ {
        let mut block = Block {
            networks: HashMap::new(),
            statements: Vec::new()
        };
        for item in items {
            match item {
                StatementOrDecl::Statement(stm) => block.statements.push(stm),
                StatementOrDecl::Decl(id, net) => {block.networks.insert(id, net);}
            }
        }
        block
    }

    rule statement_or_decl(ctx: &mut Ctx) -> StatementOrDecl =
        a:statement(ctx) {StatementOrDecl::Statement(a)} /
        a:net_decl(ctx) {StatementOrDecl::Decl(a.0, a.1)}

    rule net_decl(ctx: &mut Ctx) -> (Ident, Network) =
        "net" _ "(" _ color:color() ")" _ name:ident(ctx) "[" _ signals:ident(ctx)+ "]" _
    {
        (name, Network{color, signals})
    }

    rule color() -> Color = red() / green()
    rule red() -> Color = "red" _ {Red}
    rule green() -> Color = "green" _ {Green}

    rule statement(ctx: &mut Ctx) -> Statement =
        combinator:(arith(ctx)/decider(ctx)) {Statement::Combinator(combinator)} /
        instance:instance(ctx) {Statement::Instance(instance)} /
        looping:looping(ctx) {Statement::Loop(looping)} /
        block:block(ctx) {Statement::Block(block)}

    rule arith(ctx: &mut Ctx) -> Combinator =
        input:connector(ctx)
        "[" _ left:abstract_signal(ctx)
        op: arithmetic_op()
        right:signal_or_const(ctx) "]" _
        "->" _
        output:connector(ctx)
        "[" _ out:abstract_signal(ctx) "]" _ {
            Combinator{
                input,
                output,
                specifics: CombinatorType::Arithmetic {
                    left,
                    right,
                    out,
                    op
                }
            }
    }

    rule decider(ctx: &mut Ctx) -> Combinator =
        input:connector(ctx)
        "[" _ left:abstract_signal(ctx)
        op: decide_op()
        right:signal_or_const(ctx) "]" _
        "->" _
        output_kind: $("value" / "one") _
        output:connector(ctx)
        "[" _ out:abstract_signal(ctx) "]" _ {?
            Ok(Combinator{
                input,
                output,
                specifics: CombinatorType::Decider {
                    left,
                    right,
                    out,
                    op,
                    output_one: match output_kind {
                        "one" => true,
                        "value" => false,
                        _ => return Err("expected 'one' or 'value'")
                    }
                }
            })
    }

    // During parsing, no info about the networks is available.
    // Colors will be wrong untill they get fixed up.
    rule connector(ctx: &mut Ctx) -> Connector =
        name:ident(ctx) {
            Connector {red: Some(name), green: None}
        } /
        "(" _ first:ident(ctx) second:ident(ctx) ")" _ {
                Connector {red: Some(first), green: Some(second)}
        }

    rule instance(ctx: &mut Ctx) -> Instance =
        name:ident(ctx)
        params:("<" _ items:expr(ctx)* ">" {items})? _
        "(" _ args:arg_net(ctx)+ ")" _
    {
        Instance {
            name,
            params: params.unwrap_or_default(),
            args
        }
    }

    rule arg_net(ctx: &mut Ctx) -> ArgNet = name:ident(ctx) _ "[" _ signals:arg_signal(ctx)+ "]" _ {
        let mut signal_map = HashMap::new();
        for (argname, localname) in signals {
            signal_map.insert(argname, localname);
        }
        ArgNet {
            name,
            signal_map
        }
    }

    rule arg_signal(ctx: &mut Ctx) -> (Ident, Ident) = argname: (a:ident(ctx) _ ":"{a})? _ name: ident(ctx) {
        (argname.unwrap_or(name), name)
    }

    rule looping(ctx: &mut Ctx) -> Loop =
        "loop" _ iter:ident(ctx)
        "from" _ min:expr(ctx)
        "to" _ max:expr(ctx)
        body:block(ctx)
    {
        Loop { iter, min, max, body }
    }

    rule ident(ctx: &mut Ctx) -> Ident = quiet!{name:$(['a'..='z'| 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) _ {
        ctx.intern(name)
    }} / expected!("identifier")

    rule arithmetic_op() -> ArithOp = op: $("+" / "-" / "*" / "/" / "%" / "**" / "<<" / ">>" / "&" / "|" / "^") _ {
        match op {
            "+" => ArithOp::Add,
            "-" => ArithOp::Sub,
            "*" => ArithOp::Mul,
            "/" => ArithOp::Div,
            "%" => ArithOp::Mod,
            "**" => ArithOp::Exp,
            "<<" => ArithOp::ShiftLeft,
            ">>" => ArithOp::ShiftRight,
            "&" => ArithOp::And,
            "|" => ArithOp::Or,
            "^" => ArithOp::Xor,
            _ => unreachable!()
        }
    }

    rule decide_op() -> DecideOp = op: $(">" / "<" / "==" / ">=" / "<=" / "!=") _ {
        match op {
            ">" => DecideOp::Greater,
            "<" => DecideOp::Less,
            "==" => DecideOp::Equal,
            ">=" => DecideOp::GreaterOrEqual,
            "<=" => DecideOp::LessOrEqual,
            "!=" => DecideOp::NotEqual,
            _ => unreachable!()
        }
    }

    rule abstract_signal(ctx: &mut Ctx) -> AbstractSignal =
        "any" _ {AbstractSignal::Any} /
        "every" _ {AbstractSignal::Every} /
        "each" _ {AbstractSignal::Each} /
        name:ident(ctx) {AbstractSignal::Signal(name)}

    rule signal_or_const(ctx: &mut Ctx) -> SignalOrConst =
        signal:ident(ctx) {SignalOrConst::Signal(signal)} /
        num:number() {SignalOrConst::ConstValue(num)}

    rule expr(ctx: &mut Ctx) -> Expr =
        num:number() {Expr::Literal(num)} /
        param:ident(ctx) {Expr::Param(param)}

    rule number() -> i32 = quiet!{minus: "-"? num: (hexadecimal() / decimal()) _ {
        if minus.is_some() {
            -num
        } else {
            num
        }
    }} / expected!("number")

    rule decimal() -> i32 = num: $(['0'..='9']+) {?
        num.parse().or(Err("Overflowing integer literal"))
    }

    rule hexadecimal() -> i32 = "0x" num: $(['0'..='9' | 'a'..='f' | 'A'..='F']+) {?
        i32::from_str_radix(num, 16).or(Err("Overflowing integer literal"))
    }
}}
