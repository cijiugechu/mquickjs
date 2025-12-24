//! JS-facing math helpers ported from `mquickjs-c/libm.{c,h}`.

use crate::softfp::{sf64, RoundingMode};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum RintMode {
    Rdn,
    Rup,
    Rtz,
    Rmm,
    RmmUp,
}

fn rint_sf64(a: f64, rm: RintMode) -> f64 {
    let mut u = a.to_bits();
    let e = ((u >> 52) & 0x7ff) as i32 - 0x3ff;
    let s = (u >> 63) & 1;

    if e < 0 {
        let m = u & ((1u64 << 52) - 1);
        if e == -0x3ff && m == 0 {
            return f64::from_bits(u);
        }
        let one = 0x3ffu64 << 52;
        u = 0;
        match rm {
            RintMode::Rup => {
                if s == 0 {
                    u = one;
                }
            }
            RintMode::Rdn => {
                if s != 0 {
                    u = one;
                }
            }
            RintMode::Rmm | RintMode::RmmUp => {
                if e == -1 && (m != 0 || s == 0 || rm == RintMode::Rmm) {
                    u = one;
                }
            }
            RintMode::Rtz => {}
        }
        u |= s << 63;
    } else if e < 52 {
        let one = 1u64 << (52 - e);
        let frac_mask = one - 1;
        let addend = match rm {
            RintMode::RmmUp => (one >> 1).wrapping_sub(s),
            RintMode::Rmm => one >> 1,
            RintMode::Rtz => 0,
            RintMode::Rup => {
                if s == 0 {
                    one - 1
                } else {
                    0
                }
            }
            RintMode::Rdn => {
                if s != 0 {
                    one - 1
                } else {
                    0
                }
            }
        };
        u = u.wrapping_add(addend);
        u &= !frac_mask;
    }

    f64::from_bits(u)
}

pub fn js_floor(x: f64) -> f64 {
    rint_sf64(x, RintMode::Rdn)
}

pub fn js_ceil(x: f64) -> f64 {
    rint_sf64(x, RintMode::Rup)
}

pub fn js_trunc(x: f64) -> f64 {
    rint_sf64(x, RintMode::Rtz)
}

pub fn js_round_inf(x: f64) -> f64 {
    rint_sf64(x, RintMode::RmmUp)
}

pub fn js_fabs(x: f64) -> f64 {
    f64::from_bits(x.to_bits() & 0x7fff_ffff_ffff_ffff)
}

pub fn js_sqrt(x: f64) -> f64 {
    ::libm::sqrt(x)
}

pub fn js_lrint(a: f64) -> i32 {
    sf64::cvt_sf64_i32(a.to_bits(), RoundingMode::Rne)
}

pub fn js_scalbn(x: f64, n: i32) -> f64 {
    const TWO54: f64 = 1.801_439_850_948_198_4e16;
    const TWOM54: f64 = 5.551_115_123_125_783e-17;
    const TINY: f64 = 1.0e-300;
    const HUGE: f64 = 1.0e300;

    let mut hx = (x.to_bits() >> 32) as u32;
    let lx = x.to_bits() as u32;
    let mut k = ((hx & 0x7ff0_0000) >> 20) as i32;
    let mut x = x;

    if k == 0 {
        if (lx | (hx & 0x7fff_ffff)) == 0 {
            return x;
        }
        x *= TWO54;
        hx = (x.to_bits() >> 32) as u32;
        k = ((hx & 0x7ff0_0000) >> 20) as i32 - 54;
        if n < -50_000 {
            return TINY * x;
        }
    }
    if k == 0x7ff {
        return x + x;
    }
    k += n;
    if k > 0x7fe {
        return HUGE * HUGE.copysign(x);
    }
    if k > 0 {
        let bits = (x.to_bits() & 0xffff_ffff) | ((hx & 0x800f_ffff) as u64) << 32;
        return f64::from_bits(bits | ((k as u64) << 52));
    }
    if k <= -54 {
        if n > 50_000 {
            return HUGE * HUGE.copysign(x);
        }
        return TINY * TINY.copysign(x);
    }
    k += 54;
    let bits = (x.to_bits() & 0xffff_ffff) | ((hx & 0x800f_ffff) as u64) << 32;
    let x = f64::from_bits(bits | ((k as u64) << 52));
    x * TWOM54
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn rint_basic_modes() {
        assert_eq!(js_floor(1.9), 1.0);
        assert_eq!(js_floor(-0.1), -1.0);
        assert_eq!(js_ceil(1.1), 2.0);
        assert_eq!(js_ceil(-1.1), -1.0);
        assert_eq!(js_trunc(1.9), 1.0);
        assert_eq!(js_trunc(-1.9), -1.0);

        let neg_zero = js_trunc(-0.5);
        assert_eq!(neg_zero, 0.0);
        assert_eq!(neg_zero.to_bits(), (-0.0f64).to_bits());
    }

    #[test]
    fn round_inf_ties_to_pos_inf() {
        assert_eq!(js_round_inf(1.5), 2.0);
        assert_eq!(js_round_inf(-1.5), -1.0);
        let neg_zero = js_round_inf(-0.5);
        assert_eq!(neg_zero, 0.0);
        assert_eq!(neg_zero.to_bits(), (-0.0f64).to_bits());
    }

    #[test]
    fn fabs_preserves_payload() {
        assert_eq!(js_fabs(-3.5), 3.5);
        let neg_zero = js_fabs(-0.0);
        assert_eq!(neg_zero.to_bits(), 0.0f64.to_bits());

        let nan = f64::from_bits(0xfff8_0000_0000_0001);
        let res = js_fabs(nan);
        assert!(res.is_nan());
        assert_eq!(res.to_bits() >> 63, 0);
    }

    #[test]
    fn lrint_matches_round_to_even() {
        assert_eq!(js_lrint(1.5), 2);
        assert_eq!(js_lrint(2.5), 2);
        assert_eq!(js_lrint(-1.5), -2);
        assert_eq!(js_lrint(f64::NAN), i32::MAX);
        assert_eq!(js_lrint(f64::INFINITY), i32::MAX);
        assert_eq!(js_lrint(f64::NEG_INFINITY), i32::MIN);
    }

    #[test]
    fn scalbn_behavior() {
        assert_eq!(js_scalbn(1.5, 2), 6.0);
        assert_eq!(js_scalbn(1.0, -1), 0.5);

        let neg_zero = js_scalbn(-0.0, 5);
        assert_eq!(neg_zero.to_bits(), (-0.0f64).to_bits());

        let nan = js_scalbn(f64::NAN, 3);
        assert!(nan.is_nan());

        let sub = f64::from_bits(1);
        assert_eq!(js_scalbn(sub, 1).to_bits(), 2);

        let neg_inf = js_scalbn(-1.0, 1024);
        assert!(neg_inf.is_infinite());
        assert!(neg_inf.is_sign_negative());
    }

    #[test]
    fn sqrt_matches_ieee() {
        assert_eq!(js_sqrt(4.0), 2.0);
        assert!(js_sqrt(-1.0).is_nan());
    }
}
