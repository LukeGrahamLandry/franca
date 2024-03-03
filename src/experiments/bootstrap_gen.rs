#![allow(non_snake_case)]
        #![allow(unused)]
        #![allow(non_upper_case_globals)]
        #![allow(clippy::no_effect)]
        #![allow(clippy::explicit_auto_deref)]
        #![allow(clippy::deref_addrof)]



struct ShiftTy {
    LSL: i64,
    LSR: i64,
    ASR: i64,
}

const Shift: &ShiftTy = &ShiftTy {
    LSL: 0b00,
    LSR: 0b01,
    ASR: 0b10,
};

#[rustfmt::skip]
fn add_Ty385Ty4(a: i64, b: i64, ) -> i64 {
    a + b
}

#[rustfmt::skip]
fn sub_Ty385Ty4(a: i64, b: i64, ) -> i64 {
    a - b
}

#[rustfmt::skip]
fn mul_Ty385Ty4(a: i64, b: i64, ) -> i64 {
    a * b
}

#[rustfmt::skip]
fn le_Ty385Ty5(a: i64, b: i64, ) -> bool {
    a <= b
}

#[rustfmt::skip]
fn shift_left_Ty385Ty4(value: i64, shift_amount: i64, ) -> i64 {
    value << shift_amount
}

#[rustfmt::skip]
fn bit_or_Ty385Ty4(a: i64, b: i64, ) -> i64 {
    a | b
}

#[rustfmt::skip]
fn bit_not_Ty4Ty4(a: i64, ) -> i64 {
    !a
}

#[rustfmt::skip]
fn bit_and_Ty385Ty4(a: i64, b: i64, ) -> i64 {
    a & b
}

#[rustfmt::skip]
fn neg_Ty4Ty4(a: i64, ) -> i64 {
    sub_Ty385Ty4(0, a)
}

#[rustfmt::skip]
fn signed_truncate_Ty385Ty4(x: i64, bit_count: i64, ) -> i64 {
    { let mut mask = sub_Ty385Ty4(shift_left_Ty385Ty4(1, bit_count), 1);
        (if le_Ty385Ty5(x, 0) { { ();
            {
                bit_and_Ty385Ty4(add_Ty385Ty4(bit_not_Ty4Ty4(mul_Ty385Ty4(x, neg_Ty4Ty4(1))), 1), mask) } } } else { { ();
            x } }) }
}

#[rustfmt::skip]
pub fn add_sr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(11, 24)), shift_left_Ty385Ty4(shift, 22)), shift_left_Ty385Ty4(0, 21)), shift_left_Ty385Ty4(a, 16)), shift_left_Ty385Ty4(amount, 10)), shift_left_Ty385Ty4(b, 5)), shift_left_Ty385Ty4(dest, 0))
}

#[rustfmt::skip]
pub fn add_im(sf: i64, dest: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(34, 23)), shift_left_Ty385Ty4(lsl_12, 22)), shift_left_Ty385Ty4(imm, 10)), shift_left_Ty385Ty4(src, 5)), shift_left_Ty385Ty4(dest, 0))
}

#[rustfmt::skip]
pub fn orr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(42, 24)), shift_left_Ty385Ty4(shift, 22)), shift_left_Ty385Ty4(0, 21)), shift_left_Ty385Ty4(a, 16)), shift_left_Ty385Ty4(amount, 10)), shift_left_Ty385Ty4(b, 5)), shift_left_Ty385Ty4(dest, 0))
}

#[rustfmt::skip]
pub fn sub_sr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(75, 24)), shift_left_Ty385Ty4(shift, 22)), shift_left_Ty385Ty4(0, 21)), shift_left_Ty385Ty4(a, 16)), shift_left_Ty385Ty4(amount, 10)), shift_left_Ty385Ty4(b, 5)), shift_left_Ty385Ty4(dest, 0))
}

#[rustfmt::skip]
pub fn sub_im(sf: i64, dest: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(162, 23)), shift_left_Ty385Ty4(lsl_12, 22)), shift_left_Ty385Ty4(imm, 10)), shift_left_Ty385Ty4(src, 5)), shift_left_Ty385Ty4(dest, 0))
}

#[rustfmt::skip]
pub fn mov(sf: i64, dest: i64, src: i64, ) -> i64 {
    orr(sf, dest, src, 31, (*(&(*Shift).LSL)), 0)
}

#[rustfmt::skip]
pub fn cbz(sf: i64, offset: i64, val: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(52, 24)), shift_left_Ty385Ty4(signed_truncate_Ty385Ty4(offset, 19), 5)), shift_left_Ty385Ty4(val, 0))
}

#[rustfmt::skip]
pub fn b(offset: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(5, 26)), shift_left_Ty385Ty4(signed_truncate_Ty385Ty4(offset, 26), 0))
}

#[rustfmt::skip]
pub fn movz(sf: i64, dest: i64, imm: i64, hw: i64, ) -> i64 {
    {
        bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(165, 23)), shift_left_Ty385Ty4(hw, 21)), shift_left_Ty385Ty4(imm, 5)), shift_left_Ty385Ty4(dest, 0)) }
}

#[rustfmt::skip]
pub fn ret(_: (), ) -> i64 {
    3596551104
}

#[rustfmt::skip]
pub fn str_uo(sf: i64, src: i64, addr: i64, offset_words: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(1, 31)), shift_left_Ty385Ty4(sf, 30)), shift_left_Ty385Ty4(228, 22)), shift_left_Ty385Ty4(offset_words, 10)), shift_left_Ty385Ty4(addr, 5)), shift_left_Ty385Ty4(src, 0))
}

#[rustfmt::skip]
pub fn ldr_uo(sf: i64, dest: i64, addr: i64, offset_words: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(1, 31)), shift_left_Ty385Ty4(sf, 30)), shift_left_Ty385Ty4(229, 22)), shift_left_Ty385Ty4(offset_words, 10)), shift_left_Ty385Ty4(addr, 5)), shift_left_Ty385Ty4(dest, 0))
}

#[rustfmt::skip]
pub fn ldp_so(sf: i64, dest1: i64, dest2: i64, addr: i64, offset_words: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(165, 22)), shift_left_Ty385Ty4(signed_truncate_Ty385Ty4(offset_words, 7), 15)), shift_left_Ty385Ty4(dest2, 10)), shift_left_Ty385Ty4(addr, 5)), shift_left_Ty385Ty4(dest1, 0))
}

#[rustfmt::skip]
pub fn stp_so(sf: i64, src1: i64, src2: i64, addr: i64, offset_words: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(sf, 31)), shift_left_Ty385Ty4(164, 22)), shift_left_Ty385Ty4(signed_truncate_Ty385Ty4(offset_words, 7), 15)), shift_left_Ty385Ty4(src2, 10)), shift_left_Ty385Ty4(addr, 5)), shift_left_Ty385Ty4(src1, 0))
}

#[rustfmt::skip]
pub fn br(addr: i64, set_link: i64, ) -> i64 {
    bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(bit_or_Ty385Ty4(0, shift_left_Ty385Ty4(856, 22)), shift_left_Ty385Ty4(set_link, 21)), shift_left_Ty385Ty4(1984, 10)), shift_left_Ty385Ty4(addr, 5)), shift_left_Ty385Ty4(0, 0))
}

pub const BOOTSTRAP: &str = r###"

@c_call fn add(a: i64, b: i64, ) i64 = (
    0x8b000020, 0xd65f03c0,
)!asm;

@c_call fn sub(a: i64, b: i64, ) i64 = (
    0xcb010000, 0xd65f03c0,
)!asm;

"###;
