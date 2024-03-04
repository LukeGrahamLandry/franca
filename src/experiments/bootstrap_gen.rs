
#![allow(non_snake_case)]
#![allow(unused)]
#![allow(non_upper_case_globals)]
#![allow(clippy::no_effect)]
#![allow(clippy::explicit_auto_deref)]
#![allow(clippy::deref_addrof)]

struct ShiftTy {
    LSL: i64,
    LSR: i64,
    ASR: i64
}

const Shift: &ShiftTy = &ShiftTy {
    LSL: 0b00,
    LSR: 0b01,
    ASR: 0b10,
};




#[rustfmt::skip]
fn add(a: i64, b: i64, ) -> i64 { 
a + b
}

#[rustfmt::skip]
fn sub(a: i64, b: i64, ) -> i64 { 
a - b
}

#[rustfmt::skip]
fn mul(a: i64, b: i64, ) -> i64 { 
a * b
}

#[rustfmt::skip]
fn le(a: i64, b: i64, ) -> bool { 
a <= b
}

#[rustfmt::skip]
fn shift_left(value: i64, shift_amount: i64, ) -> i64 { 
value << shift_amount
}

#[rustfmt::skip]
fn bit_or(a: i64, b: i64, ) -> i64 { 
a | b
}

#[rustfmt::skip]
fn bit_not(a: i64, ) -> i64 { 
!a
}

#[rustfmt::skip]
fn bit_and(a: i64, b: i64, ) -> i64 { 
a & b
}

#[rustfmt::skip]
fn neg(a: i64, ) -> i64 { 
sub(0, a)
}

#[rustfmt::skip]
fn signed_truncate(x: i64, bit_count: i64, ) -> i64 { 
{ let mut mask = sub(shift_left(1, bit_count), 1);
 (if le(x, 0) { { ();
 { 
 bit_and(add(bit_not(mul(x, neg(1))), 1), mask) } } } else { { ();
 x } }) }
}

#[rustfmt::skip]
pub fn add_sr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(11, 24)), shift_left(shift, 22)), shift_left(0, 21)), shift_left(a, 16)), shift_left(amount, 10)), shift_left(b, 5)), shift_left(dest, 0))
}

#[rustfmt::skip]
pub fn add_im(sf: i64, dest: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(34, 23)), shift_left(lsl_12, 22)), shift_left(imm, 10)), shift_left(src, 5)), shift_left(dest, 0))
}

#[rustfmt::skip]
pub fn orr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(42, 24)), shift_left(shift, 22)), shift_left(0, 21)), shift_left(a, 16)), shift_left(amount, 10)), shift_left(b, 5)), shift_left(dest, 0))
}

#[rustfmt::skip]
pub fn sub_sr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(75, 24)), shift_left(shift, 22)), shift_left(0, 21)), shift_left(b, 16)), shift_left(amount, 10)), shift_left(a, 5)), shift_left(dest, 0))
}

#[rustfmt::skip]
pub fn sub_im(sf: i64, dest: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(162, 23)), shift_left(lsl_12, 22)), shift_left(imm, 10)), shift_left(src, 5)), shift_left(dest, 0))
}

#[rustfmt::skip]
pub fn cmp(sf: i64, a: i64, b: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(107, 24)), shift_left((*(&(*Shift).LSL)), 22)), shift_left(0, 21)), shift_left(b, 16)), shift_left(0, 10)), shift_left(a, 5)), shift_left(31, 0))
}

#[rustfmt::skip]
pub fn cmp_im(sf: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(226, 23)), shift_left(lsl_12, 22)), shift_left(imm, 10)), shift_left(src, 5)), shift_left(31, 0))
}

#[rustfmt::skip]
pub fn mov(sf: i64, dest: i64, src: i64, ) -> i64 { 
orr(sf, dest, src, 31, (*(&(*Shift).LSL)), 0)
}

#[rustfmt::skip]
pub fn cbz(sf: i64, offset: i64, val: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(52, 24)), shift_left(signed_truncate(offset, 19), 5)), shift_left(val, 0))
}

#[rustfmt::skip]
pub fn cbnz(sf: i64, offset: i64, val: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(53, 24)), shift_left(signed_truncate(offset, 19), 5)), shift_left(val, 0))
}

#[rustfmt::skip]
pub fn b(offset: i64, set_link: i64, ) -> i64 { 
bit_or(bit_or(bit_or(0, shift_left(set_link, 31)), shift_left(5, 26)), shift_left(signed_truncate(offset, 26), 0))
}

#[rustfmt::skip]
pub fn b_cond(offset: i64, cond: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(0, shift_left(84, 24)), shift_left(signed_truncate(offset, 19), 5)), shift_left(0, 4)), shift_left(cond, 0))
}

#[rustfmt::skip]
pub fn movz(sf: i64, dest: i64, imm: i64, hw: i64, ) -> i64 { 
{ 
 bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(165, 23)), shift_left(hw, 21)), shift_left(imm, 5)), shift_left(dest, 0)) }
}

#[rustfmt::skip]
pub fn ret(_: (), ) -> i64 { 
3596551104
}

#[rustfmt::skip]
pub fn str_uo(sf: i64, src: i64, addr: i64, offset_words: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(1, 31)), shift_left(sf, 30)), shift_left(228, 22)), shift_left(offset_words, 10)), shift_left(addr, 5)), shift_left(src, 0))
}

#[rustfmt::skip]
pub fn ldr_uo(sf: i64, dest: i64, addr: i64, offset_words: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(1, 31)), shift_left(sf, 30)), shift_left(229, 22)), shift_left(offset_words, 10)), shift_left(addr, 5)), shift_left(dest, 0))
}

#[rustfmt::skip]
pub fn ldp_so(sf: i64, dest1: i64, dest2: i64, addr: i64, offset_words: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(165, 22)), shift_left(signed_truncate(offset_words, 7), 15)), shift_left(dest2, 10)), shift_left(addr, 5)), shift_left(dest1, 0))
}

#[rustfmt::skip]
pub fn stp_so(sf: i64, src1: i64, src2: i64, addr: i64, offset_words: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(sf, 31)), shift_left(164, 22)), shift_left(signed_truncate(offset_words, 7), 15)), shift_left(src2, 10)), shift_left(addr, 5)), shift_left(src1, 0))
}

#[rustfmt::skip]
pub fn br(addr: i64, set_link: i64, ) -> i64 { 
bit_or(bit_or(bit_or(bit_or(bit_or(0, shift_left(856, 22)), shift_left(set_link, 21)), shift_left(1984, 10)), shift_left(addr, 5)), shift_left(0, 0))
}

#[rustfmt::skip]
pub fn brk(context: i64, ) -> i64 { 
bit_or(bit_or(bit_or(0, shift_left(1697, 21)), shift_left(context, 5)), shift_left(0, 0))
}


