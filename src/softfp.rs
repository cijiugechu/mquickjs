//! Soft floating-point helpers ported from `softfp_template_icvt.h`.

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RoundingMode {
    Rne,
    Rtz,
    Rdn,
    Rup,
    Rmm,
    RmmUp,
}

fn rounding_addend(rm: RoundingMode, sign: u32, rnd_size: u32) -> u64 {
    match rm {
        RoundingMode::Rne | RoundingMode::Rmm => 1u64 << (rnd_size - 1),
        RoundingMode::Rtz => 0,
        RoundingMode::Rdn => {
            if sign == 1 {
                (1u64 << rnd_size) - 1
            } else {
                0
            }
        }
        RoundingMode::Rup | RoundingMode::RmmUp => {
            if sign == 0 {
                (1u64 << rnd_size) - 1
            } else {
                0
            }
        }
    }
}

const F64_EXP_SIZE: u32 = 11;
const F64_MANT_SIZE: u32 = 52;
const F64_IMANT_SIZE: u32 = 62;
const F64_RND_SIZE: u32 = F64_IMANT_SIZE - F64_MANT_SIZE;
const F64_EXP_MASK: u64 = (1u64 << F64_EXP_SIZE) - 1;
const F64_MANT_MASK: u64 = (1u64 << F64_MANT_SIZE) - 1;
const F64_SIGN_MASK: u64 = 1u64 << 63;
const F64_EXP_MASK_SHIFTED: u64 = F64_EXP_MASK << F64_MANT_SIZE;
const F64_QNAN: u64 = F64_EXP_MASK_SHIFTED | (1u64 << (F64_MANT_SIZE - 1));

const F32_EXP_SIZE: u32 = 8;
const F32_MANT_SIZE: u32 = 23;
const F32_IMANT_SIZE: u32 = 30;
const F32_RND_SIZE: u32 = F32_IMANT_SIZE - F32_MANT_SIZE;
const F32_EXP_MASK: u32 = (1u32 << F32_EXP_SIZE) - 1;
const F32_MANT_MASK: u32 = (1u32 << F32_MANT_SIZE) - 1;
const F32_QNAN: u32 = (F32_EXP_MASK << F32_MANT_SIZE) | (1u32 << (F32_MANT_SIZE - 1));

fn pack_sf64(sign: u64, exp: i32, mant: u64) -> u64 {
    (sign << 63) | ((exp as u64) << F64_MANT_SIZE) | (mant & F64_MANT_MASK)
}

fn rshift_rnd_sf64(a: u64, d: i32) -> u64 {
    if d <= 0 {
        return a;
    }
    if d >= 64 {
        return if a != 0 { 1 } else { 0 };
    }
    let d = d as u32;
    let mask = (1u64 << d) - 1;
    (a >> d) | if (a & mask) != 0 { 1 } else { 0 }
}

fn round_pack_sf64(sign: u64, mut exp: i32, mut mant: u64, rm: RoundingMode) -> u64 {
    let rnd_size = F64_RND_SIZE;
    let addend = rounding_addend(rm, sign as u32, rnd_size);
    let rnd_mask = (1u64 << rnd_size) - 1;

    let rnd_bits;
    if exp <= 0 {
        let diff = 1 - exp;
        mant = rshift_rnd_sf64(mant, diff);
        rnd_bits = mant & rnd_mask;
        exp = 1;
    } else {
        rnd_bits = mant & rnd_mask;
    }

    mant = (mant + addend) >> rnd_size;
    if rm == RoundingMode::Rne && rnd_bits == (1u64 << (rnd_size - 1)) {
        mant &= !1;
    }

    exp += (mant >> (F64_MANT_SIZE + 1)) as i32;
    if mant <= F64_MANT_MASK {
        exp = 0;
    } else if exp >= F64_EXP_MASK as i32 {
        if addend == 0 {
            exp = (F64_EXP_MASK - 1) as i32;
            mant = F64_MANT_MASK;
        } else {
            exp = F64_EXP_MASK as i32;
            mant = 0;
        }
    }

    pack_sf64(sign, exp, mant)
}

fn normalize_sf64(sign: u64, exp: i32, mant: u64, rm: RoundingMode) -> u64 {
    if mant == 0 {
        return pack_sf64(sign, 0, 0);
    }
    let shift = mant.leading_zeros() as i32 - (64 - 1 - F64_IMANT_SIZE as i32);
    debug_assert!(shift >= 0);
    let exp = exp - shift;
    let mant = mant << shift as u32;
    round_pack_sf64(sign, exp, mant, rm)
}

fn normalize_subnormal_sf64(exp: &mut i32, mant: u64) -> u64 {
    debug_assert!(mant != 0);
    let shift = F64_MANT_SIZE as i32 - (64 - 1 - mant.leading_zeros() as i32);
    *exp = 1 - shift;
    mant << shift as u32
}

fn pack_sf32(sign: u32, exp: i32, mant: u64) -> u32 {
    (sign << 31) | ((exp as u32) << F32_MANT_SIZE) | (mant as u32 & F32_MANT_MASK)
}

fn unpack_sf32(sign: &mut u32, exp: &mut i32, a: u32) -> u32 {
    *sign = a >> 31;
    *exp = ((a >> F32_MANT_SIZE) & F32_EXP_MASK) as i32;
    a & F32_MANT_MASK
}

fn rshift_rnd_sf32(a: u64, d: i32) -> u64 {
    if d <= 0 {
        return a;
    }
    if d >= 32 {
        return if a != 0 { 1 } else { 0 };
    }
    let d = d as u32;
    let mask = (1u64 << d) - 1;
    (a >> d) | if (a & mask) != 0 { 1 } else { 0 }
}

fn round_pack_sf32(sign: u32, mut exp: i32, mut mant: u64, rm: RoundingMode) -> u32 {
    let rnd_size = F32_RND_SIZE;
    let addend = rounding_addend(rm, sign, rnd_size);
    let rnd_mask = (1u64 << rnd_size) - 1;

    let rnd_bits;
    if exp <= 0 {
        let diff = 1 - exp;
        mant = rshift_rnd_sf32(mant, diff);
        rnd_bits = mant & rnd_mask;
        exp = 1;
    } else {
        rnd_bits = mant & rnd_mask;
    }

    mant = (mant + addend) >> rnd_size;
    if rm == RoundingMode::Rne && rnd_bits == (1u64 << (rnd_size - 1)) {
        mant &= !1;
    }

    exp += (mant >> (F32_MANT_SIZE + 1)) as i32;
    if mant <= F32_MANT_MASK as u64 {
        exp = 0;
    } else if exp >= F32_EXP_MASK as i32 {
        if addend == 0 {
            exp = (F32_EXP_MASK - 1) as i32;
            mant = F32_MANT_MASK as u64;
        } else {
            exp = F32_EXP_MASK as i32;
            mant = 0;
        }
    }

    pack_sf32(sign, exp, mant)
}

fn normalize_sf32(sign: u32, exp: i32, mant: u64, rm: RoundingMode) -> u32 {
    if mant == 0 {
        return pack_sf32(sign, 0, 0);
    }
    let mant_u32 = mant as u32;
    let shift = mant_u32.leading_zeros() as i32 - (32 - 1 - F32_IMANT_SIZE as i32);
    debug_assert!(shift >= 0);
    let exp = exp - shift;
    let mant = (mant_u32 as u64) << shift as u32;
    round_pack_sf32(sign, exp, mant, rm)
}

fn normalize_subnormal_sf32(exp: &mut i32, mant: u32) -> u32 {
    debug_assert!(mant != 0);
    let shift = F32_MANT_SIZE as i32 - (32 - 1 - mant.leading_zeros() as i32);
    *exp = 1 - shift;
    mant << shift as u32
}

fn mul_u_64(a: u64, b: u64) -> (u64, u64) {
    let r = (a as u128) * (b as u128);
    ((r >> 64) as u64, r as u64)
}

fn divrem_u_64(ah: u64, al: u64, b: u64) -> (u64, u64) {
    let a = ((ah as u128) << 64) | (al as u128);
    let q = a / (b as u128);
    let r = a % (b as u128);
    (q as u64, r as u64)
}

fn sqrtrem_u_64(ah: u64, al: u64) -> (u64, bool) {
    let a = ((ah as u128) << 64) | (al as u128);
    if a == 0 {
        return (0, false);
    }
    let l = if ah != 0 {
        128 - (ah - 1).leading_zeros()
    } else {
        64 - (al - 1).leading_zeros()
    };
    let mut u = 1u128 << l.div_ceil(2);
    loop {
        let s = u;
        let next = (a / s + s) >> 1;
        if next >= s {
            let inexact = a != s * s;
            return (s as u64, inexact);
        }
        u = next;
    }
}

pub fn isnan_sf64(a: u64) -> bool {
    let exp = (a >> F64_MANT_SIZE) & F64_EXP_MASK;
    let mant = a & F64_MANT_MASK;
    exp == F64_EXP_MASK && mant != 0
}

pub fn add_sf64(mut a: u64, mut b: u64, rm: RoundingMode) -> u64 {
    if (a & !F64_SIGN_MASK) < (b & !F64_SIGN_MASK) {
        core::mem::swap(&mut a, &mut b);
    }
    let mut a_sign = (a >> 63) & 1;
    let b_sign = (b >> 63) & 1;
    let mut a_exp = ((a >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut b_exp = ((b >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut a_mant = (a & F64_MANT_MASK) << 3;
    let mut b_mant = (b & F64_MANT_MASK) << 3;

    if a_exp == F64_EXP_MASK as i32 {
        if a_mant != 0 {
            return F64_QNAN;
        }
        if b_exp == F64_EXP_MASK as i32 && a_sign != b_sign {
            return F64_QNAN;
        }
        return a;
    }
    if a_exp == 0 {
        a_exp = 1;
    } else {
        a_mant |= 1u64 << (F64_MANT_SIZE + 3);
    }
    if b_exp == 0 {
        b_exp = 1;
    } else {
        b_mant |= 1u64 << (F64_MANT_SIZE + 3);
    }
    b_mant = rshift_rnd_sf64(b_mant, a_exp - b_exp);
    if a_sign == b_sign {
        a_mant += b_mant;
    } else {
        a_mant -= b_mant;
        if a_mant == 0 {
            a_sign = u64::from(rm == RoundingMode::Rdn);
        }
    }
    a_exp += F64_RND_SIZE as i32 - 3;
    normalize_sf64(a_sign, a_exp, a_mant, rm)
}

pub fn sub_sf64(a: u64, b: u64, rm: RoundingMode) -> u64 {
    add_sf64(a, b ^ F64_SIGN_MASK, rm)
}

pub fn mul_sf64(a: u64, b: u64, rm: RoundingMode) -> u64 {
    let a_sign = (a >> 63) & 1;
    let b_sign = (b >> 63) & 1;
    let r_sign = a_sign ^ b_sign;
    let mut a_exp = ((a >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut b_exp = ((b >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut a_mant = a & F64_MANT_MASK;
    let mut b_mant = b & F64_MANT_MASK;

    if a_exp == F64_EXP_MASK as i32 || b_exp == F64_EXP_MASK as i32 {
        if isnan_sf64(a) || isnan_sf64(b) {
            return F64_QNAN;
        }
        if (a_exp == F64_EXP_MASK as i32 && b_exp == 0 && b_mant == 0)
            || (b_exp == F64_EXP_MASK as i32 && a_exp == 0 && a_mant == 0)
        {
            return F64_QNAN;
        }
        return pack_sf64(r_sign, F64_EXP_MASK as i32, 0);
    }

    if a_exp == 0 {
        if a_mant == 0 {
            return pack_sf64(r_sign, 0, 0);
        }
        a_mant = normalize_subnormal_sf64(&mut a_exp, a_mant);
    } else {
        a_mant |= 1u64 << F64_MANT_SIZE;
    }
    if b_exp == 0 {
        if b_mant == 0 {
            return pack_sf64(r_sign, 0, 0);
        }
        b_mant = normalize_subnormal_sf64(&mut b_exp, b_mant);
    } else {
        b_mant |= 1u64 << F64_MANT_SIZE;
    }

    let r_exp = a_exp + b_exp - (1i32 << (F64_EXP_SIZE - 1)) + 2;
    let (r_mant, r_mant_low) =
        mul_u_64(a_mant << F64_RND_SIZE, b_mant << (F64_RND_SIZE + 1));
    let r_mant = r_mant | u64::from(r_mant_low != 0);
    normalize_sf64(r_sign, r_exp, r_mant, rm)
}

pub fn div_sf64(a: u64, b: u64, rm: RoundingMode) -> u64 {
    let a_sign = (a >> 63) & 1;
    let b_sign = (b >> 63) & 1;
    let r_sign = a_sign ^ b_sign;
    let mut a_exp = ((a >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut b_exp = ((b >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut a_mant = a & F64_MANT_MASK;
    let mut b_mant = b & F64_MANT_MASK;

    if a_exp == F64_EXP_MASK as i32 {
        if a_mant != 0 || isnan_sf64(b) {
            return F64_QNAN;
        }
        if b_exp == F64_EXP_MASK as i32 {
            return F64_QNAN;
        }
        return pack_sf64(r_sign, F64_EXP_MASK as i32, 0);
    } else if b_exp == F64_EXP_MASK as i32 {
        if b_mant != 0 {
            return F64_QNAN;
        }
        return pack_sf64(r_sign, 0, 0);
    }

    if b_exp == 0 {
        if b_mant == 0 {
            if a_exp == 0 && a_mant == 0 {
                return F64_QNAN;
            }
            return pack_sf64(r_sign, F64_EXP_MASK as i32, 0);
        }
        b_mant = normalize_subnormal_sf64(&mut b_exp, b_mant);
    } else {
        b_mant |= 1u64 << F64_MANT_SIZE;
    }
    if a_exp == 0 {
        if a_mant == 0 {
            return pack_sf64(r_sign, 0, 0);
        }
        a_mant = normalize_subnormal_sf64(&mut a_exp, a_mant);
    } else {
        a_mant |= 1u64 << F64_MANT_SIZE;
    }

    let r_exp = a_exp - b_exp + (1i32 << (F64_EXP_SIZE - 1)) - 1;
    let (mut r_mant, r) = divrem_u_64(a_mant, 0, b_mant << 2);
    if r != 0 {
        r_mant |= 1;
    }
    normalize_sf64(r_sign, r_exp, r_mant, rm)
}

pub fn sqrt_sf64(a: u64, rm: RoundingMode) -> u64 {
    let a_sign = (a >> 63) & 1;
    let mut a_exp = ((a >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut a_mant = a & F64_MANT_MASK;

    if a_exp == F64_EXP_MASK as i32 {
        if a_mant != 0 {
            return F64_QNAN;
        }
        if a_sign != 0 {
            return F64_QNAN;
        }
        return a;
    }
    if a_sign != 0 {
        if a_exp == 0 && a_mant == 0 {
            return a;
        }
        return F64_QNAN;
    }
    if a_exp == 0 {
        if a_mant == 0 {
            return pack_sf64(0, 0, 0);
        }
        a_mant = normalize_subnormal_sf64(&mut a_exp, a_mant);
    } else {
        a_mant |= 1u64 << F64_MANT_SIZE;
    }
    a_exp -= (F64_EXP_MASK / 2) as i32;
    if a_exp & 1 != 0 {
        a_exp -= 1;
        a_mant <<= 1;
    }
    a_exp = (a_exp >> 1) + (F64_EXP_MASK / 2) as i32;
    a_mant <<= 64 - 4 - F64_MANT_SIZE;
    let (mut root, inexact) = sqrtrem_u_64(a_mant, 0);
    if inexact {
        root |= 1;
    }
    normalize_sf64(0, a_exp, root, rm)
}

pub fn eq_quiet_sf64(a: u64, b: u64) -> i32 {
    if isnan_sf64(a) || isnan_sf64(b) {
        return 0;
    }
    if ((a | b) << 1) == 0 {
        return 1;
    }
    i32::from(a == b)
}

pub fn le_sf64(a: u64, b: u64) -> i32 {
    if isnan_sf64(a) || isnan_sf64(b) {
        return 0;
    }
    let a_sign = (a >> 63) & 1;
    let b_sign = (b >> 63) & 1;
    if a_sign != b_sign {
        i32::from(a_sign != 0 || ((a | b) << 1) == 0)
    } else if a_sign != 0 {
        i32::from(a >= b)
    } else {
        i32::from(a <= b)
    }
}

pub fn lt_sf64(a: u64, b: u64) -> i32 {
    if isnan_sf64(a) || isnan_sf64(b) {
        return 0;
    }
    let a_sign = (a >> 63) & 1;
    let b_sign = (b >> 63) & 1;
    if a_sign != b_sign {
        i32::from(a_sign != 0 && ((a | b) << 1) != 0)
    } else if a_sign != 0 {
        i32::from(a > b)
    } else {
        i32::from(a < b)
    }
}

pub fn cmp_sf64(a: u64, b: u64) -> i32 {
    if isnan_sf64(a) || isnan_sf64(b) {
        return 2;
    }
    let a_sign = (a >> 63) as i32;
    let b_sign = (b >> 63) as i32;
    if a_sign != b_sign {
        if ((a | b) << 1) != 0 {
            return 1 - 2 * a_sign;
        }
        return 0;
    }
    if a < b {
        2 * a_sign - 1
    } else if a > b {
        1 - 2 * a_sign
    } else {
        0
    }
}

pub fn cvt_sf32_sf64(a: u32) -> u64 {
    let mut sign = 0u32;
    let mut exp = 0i32;
    let mut mant = unpack_sf32(&mut sign, &mut exp, a) as u64;

    if exp == F32_EXP_MASK as i32 {
        if mant != 0 {
            return F64_QNAN;
        }
        return pack_sf64(sign as u64, F64_EXP_MASK as i32, 0);
    }
    if exp == 0 {
        if mant == 0 {
            return pack_sf64(sign as u64, 0, 0);
        }
        mant = normalize_subnormal_sf32(&mut exp, mant as u32) as u64;
    }
    exp = exp - 0x7f + (F64_EXP_MASK / 2) as i32;
    mant <<= F64_MANT_SIZE - F32_MANT_SIZE;
    pack_sf64(sign as u64, exp, mant)
}

pub fn cvt_sf64_sf32(a: u64, rm: RoundingMode) -> u32 {
    let sign = (a >> 63) as u32;
    let mut exp = ((a >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut mant = a & F64_MANT_MASK;

    if exp == F64_EXP_MASK as i32 {
        if mant != 0 {
            return F32_QNAN;
        }
        return pack_sf32(sign, 0xff, 0);
    }
    if exp == 0 {
        if mant == 0 {
            return pack_sf32(sign, 0, 0);
        }
        mant = normalize_subnormal_sf64(&mut exp, mant);
    } else {
        mant |= 1u64 << F64_MANT_SIZE;
    }
    exp = exp - (F64_EXP_MASK / 2) as i32 + 0x7f;
    mant = rshift_rnd_sf64(mant, F64_MANT_SIZE as i32 - (32 - 2));
    normalize_sf32(sign, exp, mant, rm)
}

pub fn fmod_sf64(a: u64, b: u64) -> u64 {
    let a_abs = a & !F64_SIGN_MASK;
    let b_abs = b & !F64_SIGN_MASK;

    if b_abs == 0 || a_abs >= F64_EXP_MASK_SHIFTED || b_abs > F64_EXP_MASK_SHIFTED {
        return F64_QNAN;
    }
    if a_abs < b_abs {
        return a;
    }
    if a_abs == b_abs {
        return a & F64_SIGN_MASK;
    }

    let a_sign = (a >> 63) & 1;
    let mut a_exp = ((a >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut b_exp = ((b >> F64_MANT_SIZE) & F64_EXP_MASK) as i32;
    let mut a_mant = a & F64_MANT_MASK;
    let mut b_mant = b & F64_MANT_MASK;

    if a_exp == 0 {
        a_mant = normalize_subnormal_sf64(&mut a_exp, a_mant);
    } else {
        a_mant |= 1u64 << F64_MANT_SIZE;
    }
    if b_exp == 0 {
        b_mant = normalize_subnormal_sf64(&mut b_exp, b_mant);
    } else {
        b_mant |= 1u64 << F64_MANT_SIZE;
    }

    let mut n = a_exp - b_exp;
    if a_mant >= b_mant {
        a_mant -= b_mant;
    }
    while n != 0 {
        a_mant <<= 1;
        if a_mant >= b_mant {
            a_mant -= b_mant;
        }
        n -= 1;
    }

    normalize_sf64(a_sign, b_exp, a_mant << F64_RND_SIZE, RoundingMode::Rne)
}

macro_rules! define_softfp_icvt {
    (
        $mod:ident,
        $f_uint:ty,
        $f_size:expr,
        $mant_size:expr,
        $exp_size:expr,
        $cvt_sf_i32:ident,
        $cvt_sf_u32:ident,
        $cvt_sf_i64:ident,
        $cvt_sf_u64:ident,
        $cvt_i32_sf:ident,
        $cvt_u32_sf:ident,
        $cvt_i64_sf:ident,
        $cvt_u64_sf:ident
    ) => {
        pub mod $mod {
            use super::{rounding_addend, RoundingMode};

            const F_SIZE: i32 = $f_size;
            const MANT_SIZE: i32 = $mant_size;
            const F_SIZE_U32: u32 = $f_size;
            const MANT_SIZE_U32: u32 = $mant_size;
            const EXP_SIZE_U32: u32 = $exp_size;
            const EXP_MASK: u32 = (1u32 << EXP_SIZE_U32) - 1;
            const MANT_MASK: u64 = (1u64 << MANT_SIZE_U32) - 1;
            const IMANT_SIZE: i32 = F_SIZE - 2;
            const RND_SIZE: i32 = IMANT_SIZE - MANT_SIZE;

            fn pack_sf(sign: u32, exp: i32, mant: u64) -> $f_uint {
                let bits = ((sign as u64) << (F_SIZE_U32 - 1))
                    | ((exp as u64) << MANT_SIZE_U32)
                    | (mant & MANT_MASK);
                bits as $f_uint
            }

            fn rshift_rnd(a: u64, d: i32) -> u64 {
                if d <= 0 {
                    return a;
                }
                if d >= F_SIZE {
                    return if a != 0 { 1 } else { 0 };
                }
                let d = d as u32;
                let mask = (1u64 << d) - 1;
                (a >> d) | if (a & mask) != 0 { 1 } else { 0 }
            }

            fn round_pack_sf(sign: u32, mut exp: i32, mut mant: u64, rm: RoundingMode) -> $f_uint {
                let rnd_size = RND_SIZE as u32;
                let addend = rounding_addend(rm, sign, rnd_size);
                let rnd_mask = (1u64 << rnd_size) - 1;

                let rnd_bits;
                if exp <= 0 {
                    let _is_subnormal = exp < 0 || (mant + addend) < (1u64 << (F_SIZE_U32 - 1));
                    let diff = 1 - exp;
                    mant = rshift_rnd(mant, diff);
                    rnd_bits = mant & rnd_mask;
                    exp = 1;
                } else {
                    rnd_bits = mant & rnd_mask;
                }

                mant = (mant + addend) >> rnd_size;
                if rm == RoundingMode::Rne && rnd_bits == (1u64 << (rnd_size - 1)) {
                    mant &= !1;
                }

                exp += (mant >> (MANT_SIZE_U32 + 1)) as i32;
                if mant <= MANT_MASK {
                    exp = 0;
                } else if exp >= EXP_MASK as i32 {
                    if addend == 0 {
                        exp = (EXP_MASK - 1) as i32;
                        mant = MANT_MASK;
                    } else {
                        exp = EXP_MASK as i32;
                        mant = 0;
                    }
                }

                pack_sf(sign, exp, mant)
            }

            fn normalize_sf(sign: u32, exp: i32, mant: u64, rm: RoundingMode) -> $f_uint {
                if mant == 0 {
                    return pack_sf(sign, 0, 0);
                }
                let shift = (mant as $f_uint).leading_zeros() as i32 - (F_SIZE - 1 - IMANT_SIZE);
                let exp = exp - shift;
                let mant = mant << (shift as u32);
                round_pack_sf(sign, exp, mant, rm)
            }

            fn internal_cvt_sf_to_i32(bits: $f_uint, rm: RoundingMode, is_unsigned: bool) -> i32 {
                let a = bits as u64;
                let mut a_sign = (a >> (F_SIZE - 1)) as u32 & 1;
                let mut a_exp = ((a >> MANT_SIZE_U32) & (EXP_MASK as u64)) as i32;
                let mut a_mant = a & MANT_MASK;

                if a_exp == EXP_MASK as i32 && a_mant != 0 {
                    a_sign = 0;
                }
                if a_exp == 0 {
                    a_exp = 1;
                } else {
                    a_mant |= 1u64 << MANT_SIZE_U32;
                }
                a_mant <<= RND_SIZE as u32;
                a_exp = a_exp - (EXP_MASK as i32 / 2) - MANT_SIZE;

                let r_max: u32 = if is_unsigned {
                    a_sign.wrapping_sub(1)
                } else {
                    (1u32 << 31) - (a_sign ^ 1)
                };

                let mut r: u32;
                if a_exp >= 0 {
                    if a_exp <= (32 - 1 - MANT_SIZE) {
                        r = ((a_mant >> (RND_SIZE as u32)) as u32) << (a_exp as u32);
                        if r > r_max {
                            return r_max as i32;
                        }
                    } else {
                        return r_max as i32;
                    }
                } else {
                    a_mant = rshift_rnd(a_mant, -a_exp);
                    let addend = rounding_addend(rm, a_sign, RND_SIZE as u32);
                    let rnd_bits = a_mant & ((1u64 << (RND_SIZE as u32)) - 1);
                    a_mant = (a_mant + addend) >> (RND_SIZE as u32);
                    if rm == RoundingMode::Rne && rnd_bits == (1u64 << (RND_SIZE as u32 - 1)) {
                        a_mant &= !1;
                    }
                    if a_mant > r_max as u64 {
                        return r_max as i32;
                    }
                    r = a_mant as u32;
                }

                if a_sign != 0 {
                    r = r.wrapping_neg();
                }
                r as i32
            }

            fn internal_cvt_sf_to_i64(bits: $f_uint, rm: RoundingMode, is_unsigned: bool) -> i64 {
                let a = bits as u64;
                let mut a_sign = (a >> (F_SIZE - 1)) as u32 & 1;
                let mut a_exp = ((a >> MANT_SIZE_U32) & (EXP_MASK as u64)) as i32;
                let mut a_mant = a & MANT_MASK;

                if a_exp == EXP_MASK as i32 && a_mant != 0 {
                    a_sign = 0;
                }
                if a_exp == 0 {
                    a_exp = 1;
                } else {
                    a_mant |= 1u64 << MANT_SIZE_U32;
                }
                a_mant <<= RND_SIZE as u32;
                a_exp = a_exp - (EXP_MASK as i32 / 2) - MANT_SIZE;

                let r_max: u64 = if is_unsigned {
                    (a_sign as u64).wrapping_sub(1)
                } else {
                    (1u64 << 63) - (a_sign as u64 ^ 1)
                };

                let mut r: u64;
                if a_exp >= 0 {
                    if a_exp <= (64 - 1 - MANT_SIZE) {
                        r = (a_mant >> (RND_SIZE as u32)) << (a_exp as u32);
                        if r > r_max {
                            return r_max as i64;
                        }
                    } else {
                        return r_max as i64;
                    }
                } else {
                    a_mant = rshift_rnd(a_mant, -a_exp);
                    let addend = rounding_addend(rm, a_sign, RND_SIZE as u32);
                    let rnd_bits = a_mant & ((1u64 << (RND_SIZE as u32)) - 1);
                    a_mant = (a_mant + addend) >> (RND_SIZE as u32);
                    if rm == RoundingMode::Rne && rnd_bits == (1u64 << (RND_SIZE as u32 - 1)) {
                        a_mant &= !1;
                    }
                    if a_mant > r_max {
                        return r_max as i64;
                    }
                    r = a_mant;
                }

                if a_sign != 0 {
                    r = r.wrapping_neg();
                }
                r as i64
            }

            fn internal_cvt_i32_to_sf(a: i32, rm: RoundingMode, is_unsigned: bool) -> $f_uint {
                if a == 0 {
                    return pack_sf(0, 0, 0);
                }
                let (sign, mut r): (u32, u32) = if !is_unsigned && a < 0 {
                    (1, a.wrapping_neg() as u32)
                } else {
                    (0, a as u32)
                };

                let mut exp = (EXP_MASK as i32 / 2) + F_SIZE - 2;
                let l = 32 - (r.leading_zeros() as i32) - (F_SIZE - 1);
                if l > 0 {
                    let mask = r & ((1u32 << (l as u32)) - 1);
                    r = (r >> (l as u32)) | if (r & mask) != 0 { 1 } else { 0 };
                    exp += l;
                }
                normalize_sf(sign, exp, r as u64, rm)
            }

            fn internal_cvt_i64_to_sf(a: i64, rm: RoundingMode, is_unsigned: bool) -> $f_uint {
                if a == 0 {
                    return pack_sf(0, 0, 0);
                }
                let (sign, mut r): (u32, u64) = if !is_unsigned && a < 0 {
                    (1, a.wrapping_neg() as u64)
                } else {
                    (0, a as u64)
                };

                let mut exp = (EXP_MASK as i32 / 2) + F_SIZE - 2;
                let l = 64 - (r.leading_zeros() as i32) - (F_SIZE - 1);
                if l > 0 {
                    let mask = r & ((1u64 << (l as u32)) - 1);
                    r = (r >> (l as u32)) | if (r & mask) != 0 { 1 } else { 0 };
                    exp += l;
                }
                normalize_sf(sign, exp, r, rm)
            }

            pub fn $cvt_sf_i32(bits: $f_uint, rm: RoundingMode) -> i32 {
                internal_cvt_sf_to_i32(bits, rm, false)
            }

            pub fn $cvt_sf_u32(bits: $f_uint, rm: RoundingMode) -> u32 {
                internal_cvt_sf_to_i32(bits, rm, true) as u32
            }

            pub fn $cvt_sf_i64(bits: $f_uint, rm: RoundingMode) -> i64 {
                internal_cvt_sf_to_i64(bits, rm, false)
            }

            pub fn $cvt_sf_u64(bits: $f_uint, rm: RoundingMode) -> u64 {
                internal_cvt_sf_to_i64(bits, rm, true) as u64
            }

            pub fn $cvt_i32_sf(a: i32, rm: RoundingMode) -> $f_uint {
                internal_cvt_i32_to_sf(a, rm, false)
            }

            pub fn $cvt_u32_sf(a: u32, rm: RoundingMode) -> $f_uint {
                internal_cvt_i32_to_sf(a as i32, rm, true)
            }

            pub fn $cvt_i64_sf(a: i64, rm: RoundingMode) -> $f_uint {
                internal_cvt_i64_to_sf(a, rm, false)
            }

            pub fn $cvt_u64_sf(a: u64, rm: RoundingMode) -> $f_uint {
                internal_cvt_i64_to_sf(a as i64, rm, true)
            }
        }
    };
}

define_softfp_icvt!(
    sf32,
    u32,
    32,
    23,
    8,
    cvt_sf32_i32,
    cvt_sf32_u32,
    cvt_sf32_i64,
    cvt_sf32_u64,
    cvt_i32_sf32,
    cvt_u32_sf32,
    cvt_i64_sf32,
    cvt_u64_sf32
);
define_softfp_icvt!(
    sf64,
    u64,
    64,
    52,
    11,
    cvt_sf64_i32,
    cvt_sf64_u32,
    cvt_sf64_i64,
    cvt_sf64_u64,
    cvt_i32_sf64,
    cvt_u32_sf64,
    cvt_i64_sf64,
    cvt_u64_sf64
);

#[cfg(all(test, not(miri)))]
mod tests {
    use super::{
        add_sf64, cmp_sf64, cvt_sf32_sf64, cvt_sf64_sf32, div_sf64, isnan_sf64, mul_sf64,
        sf32, sf64, sqrt_sf64, RoundingMode,
    };

    #[test]
    fn cvt_sf64_i32_handles_nan_and_infinity() {
        assert_eq!(sf64::cvt_sf64_i32(f64::NAN.to_bits(), RoundingMode::Rne), i32::MAX);
        assert_eq!(sf64::cvt_sf64_i32(f64::INFINITY.to_bits(), RoundingMode::Rne), i32::MAX);
        assert_eq!(sf64::cvt_sf64_i32(f64::NEG_INFINITY.to_bits(), RoundingMode::Rne), i32::MIN);
        assert_eq!(sf64::cvt_sf64_u32((-1.0f64).to_bits(), RoundingMode::Rne), 0);
    }

    #[test]
    fn cvt_sf64_i32_rounding_modes() {
        let val = (1.5f64).to_bits();
        assert_eq!(sf64::cvt_sf64_i32(val, RoundingMode::Rne), 2);
        assert_eq!(sf64::cvt_sf64_i32(val, RoundingMode::Rtz), 1);

        let val = (-1.5f64).to_bits();
        assert_eq!(sf64::cvt_sf64_i32(val, RoundingMode::Rne), -2);
        assert_eq!(sf64::cvt_sf64_i32(val, RoundingMode::Rtz), -1);
    }

    #[test]
    fn cvt_sf32_i32_rounding_modes() {
        let val = (1.5f32).to_bits();
        assert_eq!(sf32::cvt_sf32_i32(val, RoundingMode::Rne), 2);
        assert_eq!(sf32::cvt_sf32_i32(val, RoundingMode::Rtz), 1);
    }

    #[test]
    fn cvt_i32_sf64_matches_rust_rne() {
        let bits = sf64::cvt_i32_sf64(-123, RoundingMode::Rne);
        assert_eq!(bits, (-123f64).to_bits());

        let bits = sf64::cvt_u64_sf64((1u64 << 53) + 1, RoundingMode::Rne);
        assert_eq!(bits, (((1u64 << 53) + 1) as f64).to_bits());
    }

    #[test]
    fn cvt_i32_sf32_matches_rust_rne() {
        let bits = sf32::cvt_i32_sf32(7, RoundingMode::Rne);
        assert_eq!(bits, (7f32).to_bits());
    }

    #[test]
    fn add_sf64_cancellation_respects_rounding_zero_sign() {
        let res_rdn = add_sf64(1.0f64.to_bits(), (-1.0f64).to_bits(), RoundingMode::Rdn);
        assert_eq!(res_rdn, (-0.0f64).to_bits());

        let res_rne = add_sf64(1.0f64.to_bits(), (-1.0f64).to_bits(), RoundingMode::Rne);
        assert_eq!(res_rne, 0.0f64.to_bits());
    }

    #[test]
    fn mul_sf64_zero_infinity_is_nan() {
        let res = mul_sf64(0.0f64.to_bits(), f64::INFINITY.to_bits(), RoundingMode::Rne);
        assert!(isnan_sf64(res));
    }

    #[test]
    fn div_sf64_zero_cases() {
        let res = div_sf64(0.0f64.to_bits(), 0.0f64.to_bits(), RoundingMode::Rne);
        assert!(isnan_sf64(res));

        let res = div_sf64(1.0f64.to_bits(), 0.0f64.to_bits(), RoundingMode::Rne);
        assert_eq!(res, f64::INFINITY.to_bits());
    }

    #[test]
    fn sqrt_sf64_negative_is_nan() {
        let res = sqrt_sf64((-1.0f64).to_bits(), RoundingMode::Rne);
        assert!(isnan_sf64(res));

        let neg_zero = (-0.0f64).to_bits();
        assert_eq!(sqrt_sf64(neg_zero, RoundingMode::Rne), neg_zero);
    }

    #[test]
    fn cmp_sf64_handles_nan_and_zero() {
        assert_eq!(cmp_sf64(f64::NAN.to_bits(), 0.0f64.to_bits()), 2);
        assert_eq!(cmp_sf64((-0.0f64).to_bits(), 0.0f64.to_bits()), 0);
    }

    #[test]
    fn cvt_sf32_sf64_matches_rust_cast() {
        let sub = f32::from_bits(1);
        let res = cvt_sf32_sf64(sub.to_bits());
        assert_eq!(res, (sub as f64).to_bits());
    }

    #[test]
    fn cvt_sf64_sf32_rounding_ties() {
        let half_ulp = 1.0f64 / (1u64 << 24) as f64;
        let halfway = 1.0f64 + half_ulp;
        let rne = cvt_sf64_sf32(halfway.to_bits(), RoundingMode::Rne);
        assert_eq!(rne, 1.0f32.to_bits());

        let rmm = cvt_sf64_sf32(halfway.to_bits(), RoundingMode::Rmm);
        assert_eq!(rmm, f32::from_bits(0x3f80_0001).to_bits());
    }
}
