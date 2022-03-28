use std::collections::HashMap;

use crate::ast::*;
use crate::model::{ArithOp, Color, DecideOp};

peg::parser! { pub grammar fhdl() for str {

    rule _() = quiet!{[' ' | '\n' | '\t']* ("#"[^'\n']*"\n"?_)?}

    pub rule modules(ctx: &mut Strings) -> Vec<Module> = module(ctx)*

    rule module(ctx: &mut Strings) -> Module =
        "module" _ name:ident(ctx)
        params:("<" _ items:ident(ctx)* ">" _ {items})?
        "(" _ io:net_decl(ctx)* ")" _
        body: block(ctx)
    {
        let mut args = HashMap::new();
        for net in io {
            args.insert(net.name, net);
        }
        Module {
            name: name,
            params: params.unwrap_or_default(),
            args,
            body,
        }
    }

    rule block(ctx: &mut Strings) -> Vec<Statement> = "{" _ items:statement(ctx)* "}" _ {
        items
    }

    rule statement(ctx: &mut Strings) -> Statement =
        start_pos:position!() inner:statement_inner(ctx) { Statement{ start_pos, inner } }

    rule statement_inner(ctx: &mut Strings) -> StatementInner =
        a:net_decl(ctx) {StatementInner::NetDecl(a)} /
        a:param_decl(ctx) /
        combinator:(arith(ctx)/decider(ctx)/constant(ctx)) {StatementInner::Combinator(combinator)} /
        instance:instance(ctx) {StatementInner::Instance(instance)} /
        looping:looping(ctx) {StatementInner::Loop(looping)} /
        block:block(ctx) {StatementInner::Block(block)}

    rule net_decl(ctx: &mut Strings) -> Network =
        "net" _ "(" _ color:color() ")" _ name:ident(ctx) "[" _ signals:ident(ctx)+ "]" _
    {
        Network{name, color, signals}
    }

    rule param_decl(ctx: &mut Strings) -> StatementInner =
        name:ident(ctx) "=" _ expr:expr(ctx) {StatementInner::ParamDecl(name, expr)}

    rule color() -> Color = red() / green()
    rule red() -> Color = "red" _ {Color::Red}
    rule green() -> Color = "green" _ {Color::Green}

    rule arith(ctx: &mut Strings) -> Combinator =
        output:connector(ctx)
        "[" _ out:abstract_signal(ctx) "]" _
        "<-" _
        input:connector(ctx)
        "[" _ left:abstract_signal(ctx)
        calc: (op: arithmetic_op()
        right:signal_or_const(ctx) {(op, right)})? "]" _
        {
            let (op, right) = calc.unwrap_or((ArithOp::Xor, SignalOrConst::ConstValue(Expr::Literal(0))));
            Combinator::Arithmetic{
                input,
                output,
                left,
                right,
                out,
                op
            }
    }

    rule decider(ctx: &mut Strings) -> Combinator =
        output:connector(ctx)
        "[" _ out:abstract_signal(ctx) "]" _
        "<-" _
        input:connector(ctx)
        "[" _ left:abstract_signal(ctx)
        op: decide_op()
        right:signal_or_const(ctx) "]" _
        output_kind: $("value" / "one") _
        {?
            Ok(Combinator::Decider{
                input,
                output,
                left,
                right,
                out,
                op,
                output_one: match output_kind {
                    "one" => true,
                    "value" => false,
                    _ => return Err("expected 'one' or 'value'")
                }
            })
    }

    rule constant(ctx: &mut Strings) -> Combinator =
        output:connector(ctx)
        "[" _ out:ident(ctx) "]" _
        "<-" _ value:expr(ctx)
    {
        Combinator::Constant{
            output,
            out,
            value
        }
    }

    rule connector(ctx: &mut Strings) -> Connector =
        name:ident(ctx) {
            Connector(name, None)
        } /
        "(" _ first:ident(ctx) second:ident(ctx) ")" _ {
                Connector (first, Some(second))
        }

    rule instance(ctx: &mut Strings) -> Instance =
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

    rule arg_net(ctx: &mut Strings) -> ArgNet = name:ident(ctx) _ "[" _ signals:arg_signal(ctx)+ "]" _ {
        let mut signal_map = HashMap::new();
        for (argname, localname) in signals {
            signal_map.insert(argname, localname);
        }
        ArgNet {
            name,
            signal_map
        }
    }

    rule arg_signal(ctx: &mut Strings) -> (Ident, Ident) = argname: (a:ident(ctx) _ ":"{a})? _ name: ident(ctx) {
        (argname.unwrap_or(name), name)
    }

    rule looping(ctx: &mut Strings) -> Loop =
        "loop" _ iter:ident(ctx)
        "from" _ min:expr(ctx)
        "to" _ max:expr(ctx)
        body:block(ctx)
    {
        Loop { iter, min, max, body }
    }

    rule ident(ctx: &mut Strings) -> Ident = quiet!{name:$(['a'..='z'| 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) _ {
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

    rule abstract_signal(ctx: &mut Strings) -> AbstractSignal =
        "any" _ {AbstractSignal::Any} /
        "every" _ {AbstractSignal::Every} /
        "each" _ {AbstractSignal::Each} /
        name:ident(ctx) {AbstractSignal::Signal(name)}

    rule signal_or_const(ctx: &mut Strings) -> SignalOrConst =
        signal:ident(ctx) {SignalOrConst::Signal(signal)} /
        expr:expr(ctx) {SignalOrConst::ConstValue(expr)}

    rule expr(ctx: &mut Strings) -> Expr =
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
