use crate::model::ArithOp;

pub fn calc(left: i32, right: i32, op: ArithOp) -> i32 {
    match op {
        ArithOp::Add => left.wrapping_add(right),
        ArithOp::Sub => left.wrapping_sub(right),
        ArithOp::Mul => left.wrapping_mul(right),
        ArithOp::Div => {
            if right == 0 {
                0
            } else {
                left.wrapping_div(right)
            }
        }
        ArithOp::Mod => {
            if right == 0 {
                0
            } else {
                left % right
            }
        }
        ArithOp::Exp => left.wrapping_pow(right as u32),
        ArithOp::ShiftLeft => left.wrapping_shl(right as u32),
        ArithOp::ShiftRight => left.wrapping_shr(right as u32),
        ArithOp::And => left & right,
        ArithOp::Or => left | right,
        ArithOp::Xor => left ^ right,
    }
}
