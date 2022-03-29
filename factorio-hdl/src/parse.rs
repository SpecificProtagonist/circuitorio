use crate::HashMap;

use crate::ast::*;
use crate::model::{ArithOp, Color, DecideOp};

peg::parser! { pub grammar fhdl() for str {

    rule _() = quiet!{[' ' | '\n' | '\t']* ("#"[^'\n']*"\n"?_)?}

    pub rule modules(ctx: &mut Strings) -> HashMap<Ident,Module> = modules:module(ctx)* {
        modules.into_iter().map(|m|(m.name,m)).collect()
    }

    rule module(ctx: &mut Strings) -> Module =
        _ "module" name:ident(ctx)
        params:("<" items:ident(ctx)* ">" _ {items})?
        "(" io:module_net_decl(ctx)* ")" _
        body: block(ctx)
    {
        let mut args = HashMap::default();
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

    rule module_net_decl(ctx: &mut Strings) -> Network =
        _ "net" _ "(" color:color() ")" name:ident(ctx) "[" signals:module_signal(ctx)+ "]" _
    {
        Network{name, color, signals: signals.into_iter().collect()}
    }

    rule module_signal(ctx: &mut Strings) -> (Ident, IOClass) =
        name:ident(ctx) io:("(" _ io:("in"{IOClass::In}/"out"{IOClass::Out}) _ ")" _ {io})?
    {
        (name, io.unwrap_or(IOClass::InOut))
    }

    rule block(ctx: &mut Strings) -> Vec<Statement> = _ "{" items:statement(ctx)* "}" _ {
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
        _ "net" _ "(" color:color() ")" name:ident(ctx) "[" signals:ident(ctx)+ "]" _
    {
        let signals = signals.into_iter().map(|s|(s,IOClass::InOut)).collect();
        Network{name, color, signals }
    }

    rule param_decl(ctx: &mut Strings) -> StatementInner =
        name:ident(ctx) "=" _ expr:expr(ctx) {StatementInner::ParamDecl(name, expr)}

    rule color() -> Color = red() / green()
    rule red() -> Color = "red" _ {Color::Red}
    rule green() -> Color = "green" _ {Color::Green}

    rule arith(ctx: &mut Strings) -> Combinator =
        output:connector(ctx)
        "[" _ out:abstract_signal_arith(ctx) "]" _
        "<-" _
        input:connector(ctx)
        "[" _ left:abstract_signal_arith(ctx)
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
        "[" out:abstract_signal_decide(ctx) "]" _
        "<-" _
        output_kind: $("value" / "one") _ "if" _
        input:connector(ctx)
        "[" left:abstract_signal_decide(ctx)
        op: decide_op()
        right:signal_or_const(ctx) "]" _
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
        "<-" value:expr(ctx)
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
        _ "(" first:ident(ctx) second:ident(ctx) ")" _ {
                Connector (first, Some(second))
        }

    rule instance(ctx: &mut Strings) -> Instance =
        name:ident(ctx)
        params:("<" items:expr(ctx)* ">" {items})?
        "(" args:arg_net(ctx)+ ")" _
    {
        Instance {
            name,
            params: params.unwrap_or_default(),
            args
        }
    }

    rule arg_net(ctx: &mut Strings) -> ArgNet = name:ident(ctx) "[" signals:arg_signal(ctx)+ "]" _ {
        let mut signal_map = HashMap::default();
        for (argname, localname) in signals {
            signal_map.insert(argname, localname);
        }
        ArgNet {
            name,
            signal_map
        }
    }

    rule arg_signal(ctx: &mut Strings) -> (Ident, Ident) = argname: (a:ident(ctx) ":"{a})? name: ident(ctx) {
        (argname.unwrap_or(name), name)
    }

    rule looping(ctx: &mut Strings) -> Loop =
        _ "loop" _ iter:ident(ctx)
        _ "from" _ min:expr(ctx)
        _ "to" _ max:expr(ctx)
        body:block(ctx)
    {
        Loop { iter, min, max, body }
    }

    rule ident(ctx: &mut Strings) -> Ident =
        quiet!{_ !("if"/"each"/"any"/"every") name:$(['a'..='z'| 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) _
    {
        ctx.intern(name)
    }} / expected!("identifier")

    rule arithmetic_op() -> ArithOp = _ op: $("+" / "-" / "*" / "/" / "%" / "**" / "<<" / ">>" / "&" / "|" / "^") _ {
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

    rule decide_op() -> DecideOp = _ op: $(">" / "<" / "==" / ">=" / "<=" / "!=") _ {
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

    rule abstract_signal_arith(ctx: &mut Strings) -> AbstractSignal =
        _ "each" _ {AbstractSignal::Each} /
        name:ident(ctx) {AbstractSignal::Signal(name)}

    rule abstract_signal_decide(ctx: &mut Strings) -> AbstractSignal =
        _ "any" _ {AbstractSignal::Any} /
        _ "every" _ {AbstractSignal::Every} /
        _ "each" _ {AbstractSignal::Each} /
        name:ident(ctx) {AbstractSignal::Signal(name)}

    rule signal_or_const(ctx: &mut Strings) -> SignalOrConst =
        signal:ident(ctx) {SignalOrConst::Signal(signal)} /
        expr:expr(ctx) {SignalOrConst::ConstValue(expr)}

    // Can't use the inbuild precedence! because it doesn't allow ctx mutation
    rule expr(ctx: &mut Strings) -> Expr =
        left:expr_part(ctx) "+" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Add)} /
        left:expr_part(ctx) "-" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "*" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "/" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "%" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "**" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "<<" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) ">>" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "&" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "|" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        left:expr_part(ctx) "^" right:expr_part(ctx) {Expr::Calc(left.into(), right.into(), ArithOp::Sub)} /
        expr_atom(ctx)

    rule expr_part(ctx: &mut Strings) -> Expr =
        _ "(" expr:expr(ctx) ")" _ {expr} /
        expr_atom(ctx)

    rule expr_atom(ctx: &mut Strings) -> Expr =
        num:number() {Expr::Literal(num)} /
        param:ident(ctx) {Expr::Param(param)}

    rule number() -> i32 = quiet!{_ minus: "-"? _ num: (hexadecimal() / decimal()) _ {
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
