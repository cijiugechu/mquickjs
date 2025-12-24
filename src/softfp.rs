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
    use super::{sf32, sf64, RoundingMode};

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
}
