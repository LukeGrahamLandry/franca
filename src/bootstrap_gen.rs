
#![allow(non_snake_case)]
#![allow(unused)]
#![allow(non_upper_case_globals)]
#![allow(clippy::no_effect)]
#![allow(clippy::explicit_auto_deref)]
#![allow(clippy::deref_addrof)]
#![allow(clippy::double_parens)]
#![allow(clippy::identity_op)]




#[rustfmt::skip]
fn neg(a: i64, ) -> i64 { 
(0 - a)
}

#[rustfmt::skip]
fn signed_truncate(x: i64, bit_count: i64, ) -> i64 { { let mut mask = ((1 << bit_count) - 1);
 (if (x <= 0) { { 
 ((bit_not((x * neg(1))) + 1) & mask) } } else { { 
 x } }) }
}


#[rustfmt::skip]
fn bit_not(a: i64, ) -> i64 { 
!a
}

#[rustfmt::skip]
pub fn add_im(sf: i64, dest: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 { 
((((((0 | (sf << 31)) | (34 << 23)) | (lsl_12 << 22)) | (imm << 10)) | (src << 5)) | (dest << 0))
}

#[rustfmt::skip]
pub fn orr(sf: i64, dest: i64, a: i64, b: i64, shift: i64, amount: i64, ) -> i64 { 
((((((((0 | (sf << 31)) | (42 << 24)) | (shift << 22)) | (0 << 21)) | (a << 16)) | (amount << 10)) | (b << 5)) | (dest << 0))
}


#[rustfmt::skip]
pub fn sub_im(sf: i64, dest: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 { 
((((((0 | (sf << 31)) | (162 << 23)) | (lsl_12 << 22)) | (imm << 10)) | (src << 5)) | (dest << 0))
}

#[rustfmt::skip]
pub fn cmp_im(sf: i64, src: i64, imm: i64, lsl_12: i64, ) -> i64 { 
((((((0 | (sf << 31)) | (226 << 23)) | (lsl_12 << 22)) | (imm << 10)) | (src << 5)) | (31 << 0))
}

#[rustfmt::skip]
pub fn mov(sf: i64, dest: i64, src: i64, ) -> i64 { 
orr(sf, dest, src, 31, 0, 0)
}

#[rustfmt::skip]
pub fn cbz(sf: i64, offset: i64, val: i64, ) -> i64 { 
((((0 | (sf << 31)) | (52 << 24)) | (signed_truncate(offset, 19) << 5)) | (val << 0))
}

#[rustfmt::skip]
pub fn b(offset: i64, set_link: i64, ) -> i64 { 
(((0 | (set_link << 31)) | (5 << 26)) | (signed_truncate(offset, 26) << 0))
}

#[rustfmt::skip]
pub fn b_cond(offset: i64, cond: i64, ) -> i64 { 
((((0 | (84 << 24)) | (signed_truncate(offset, 19) << 5)) | (0 << 4)) | (cond << 0))
}

#[rustfmt::skip]
pub fn movz(sf: i64, dest: i64, imm: i64, hw: i64, ) -> i64 { 
(((((0 | (sf << 31)) | (165 << 23)) | (hw << 21)) | (imm << 5)) | (dest << 0))
}

#[rustfmt::skip]
pub fn movk(sf: i64, dest: i64, imm: i64, hw: i64, ) -> i64 { 
(((((0 | (sf << 31)) | (229 << 23)) | (hw << 21)) | (imm << 5)) | (dest << 0))
}

#[rustfmt::skip]
pub fn ret(_: (), ) -> i64 { 
3596551104
}

#[rustfmt::skip]
pub fn str_uo(sf: i64, src: i64, addr: i64, offset_words: i64, ) -> i64 { 
((((((0 | (1 << 31)) | (sf << 30)) | (228 << 22)) | (offset_words << 10)) | (addr << 5)) | (src << 0))
}

#[rustfmt::skip]
pub fn ldr_uo(sf: i64, dest: i64, addr: i64, offset_words: i64, ) -> i64 { 
((((((0 | (1 << 31)) | (sf << 30)) | (229 << 22)) | (offset_words << 10)) | (addr << 5)) | (dest << 0))
}

#[rustfmt::skip]
pub fn ldp_so(sf: i64, dest1: i64, dest2: i64, addr: i64, offset_words: i64, ) -> i64 { 
((((((0 | (sf << 31)) | (165 << 22)) | (signed_truncate(offset_words, 7) << 15)) | (dest2 << 10)) | (addr << 5)) | (dest1 << 0))
}

#[rustfmt::skip]
pub fn stp_so(sf: i64, src1: i64, src2: i64, addr: i64, offset_words: i64, ) -> i64 { 
((((((0 | (sf << 31)) | (164 << 22)) | (signed_truncate(offset_words, 7) << 15)) | (src2 << 10)) | (addr << 5)) | (src1 << 0))
}

#[rustfmt::skip]
pub fn br(addr: i64, set_link: i64, ) -> i64 { 
(((((0 | (856 << 22)) | (set_link << 21)) | (1984 << 10)) | (addr << 5)) | (0 << 0))
}

#[rustfmt::skip]
pub fn brk(context: i64, ) -> i64 { 
(((0 | (1697 << 21)) | (context << 5)) | (0 << 0))
}

#[rustfmt::skip]
pub fn f_ldr_uo(sf: i64, dest: i64, addr: i64, offset_scaled: i64, ) -> i64 { 
((((((0 | (1 << 31)) | (sf << 30)) | (245 << 22)) | (offset_scaled << 10)) | (addr << 5)) | (dest << 0))
}

#[rustfmt::skip]
pub fn f_str_uo(sf: i64, src: i64, addr: i64, offset_scaled: i64, ) -> i64 { 
((((((0 | (1 << 31)) | (sf << 30)) | (244 << 22)) | (offset_scaled << 10)) | (addr << 5)) | (src << 0))
}


