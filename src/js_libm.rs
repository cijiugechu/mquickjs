#![allow(clippy::approx_constant, clippy::excessive_precision)]

//! JS-facing math helpers ported from `mquickjs-c/libm.{c,h}`.

use crate::softfp::{fmod_sf64, sf64, RoundingMode};

const ZERO: f64 = 0.0;
const ONE: f64 = 1.0;
const HALF: f64 = 0.5;
const TINY: f64 = 1.0e-300;
const HUGE: f64 = 1.0e300;

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

fn extract_words(d: f64) -> (u32, u32) {
    let bits = d.to_bits();
    ((bits >> 32) as u32, bits as u32)
}

fn get_high_word(d: f64) -> u32 {
    (d.to_bits() >> 32) as u32
}

fn set_high_word(d: f64, h: u32) -> f64 {
    let bits = (d.to_bits() & 0xffff_ffff) | ((h as u64) << 32);
    f64::from_bits(bits)
}

fn get_low_word(d: f64) -> u32 {
    d.to_bits() as u32
}

fn zero_low(x: f64) -> f64 {
    f64::from_bits(x.to_bits() & 0xffff_ffff_0000_0000)
}

fn float64_from_u32(h: u32, l: u32) -> f64 {
    f64::from_bits(((h as u64) << 32) | (l as u64))
}

fn eval_poly(x: f64, coefs: &[f64]) -> f64 {
    debug_assert!(!coefs.is_empty());
    let mut r = coefs[coefs.len() - 1];
    for coef in coefs[..coefs.len() - 1].iter().rev() {
        r = r * x + *coef;
    }
    r
}

#[inline]
#[allow(clippy::eq_op)]
fn nan_from(x: f64) -> f64 {
    x - x
}

#[inline]
#[allow(clippy::eq_op)]
fn nan_from_div(x: f64) -> f64 {
    (x - x) / (x - x)
}

#[cfg(any(target_arch = "aarch64", target_arch = "x86_64", target_arch = "x86"))]
pub fn js_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[cfg(not(any(target_arch = "aarch64", target_arch = "x86_64", target_arch = "x86")))]
pub fn js_sqrt(x: f64) -> f64 {
    let sign = 0x8000_0000u32;
    let (mut ix0, mut ix1) = extract_words(x);

    if (ix0 & 0x7ff0_0000) == 0x7ff0_0000 {
        return x * x + x;
    }
    if (ix0 as i32) <= 0 {
        if ((ix0 & !sign) | ix1) == 0 {
            return x;
        } else if (ix0 as i32) < 0 {
            return nan_from_div(x);
        }
    }

    let mut m = (ix0 >> 20) as i32;
    if m == 0 {
        while ix0 == 0 {
            m -= 21;
            ix0 |= ix1 >> 11;
            ix1 <<= 21;
        }
        let mut i = 0;
        while (ix0 & 0x0010_0000) == 0 {
            ix0 <<= 1;
            i += 1;
        }
        m -= i - 1;
        ix0 |= ix1 >> (32 - i);
        ix1 <<= i;
    }
    m -= 1023;
    ix0 = (ix0 & 0x000f_ffff) | 0x0010_0000;
    if (m & 1) != 0 {
        ix0 = ix0 + ix0 + ((ix1 & sign) >> 31);
        ix1 = ix1 + ix1;
    }
    m >>= 1;

    ix0 = ix0 + ix0 + ((ix1 & sign) >> 31);
    ix1 = ix1 + ix1;
    let mut q: u32 = 0;
    let mut q1: u32 = 0;
    let mut s0: u32 = 0;
    let mut s1: u32 = 0;
    let mut r: u32 = 0x0020_0000;

    while r != 0 {
        let t = s0 + r;
        if t <= ix0 {
            s0 = t + r;
            ix0 -= t;
            q += r;
        }
        ix0 = ix0 + ix0 + ((ix1 & sign) >> 31);
        ix1 = ix1 + ix1;
        r >>= 1;
    }

    r = sign;
    while r != 0 {
        let t1 = s1 + r;
        let t = s0;
        if (t < ix0) || ((t == ix0) && (t1 <= ix1)) {
            s1 = t1 + r;
            if (t1 & sign) == sign && (s1 & sign) == 0 {
                s0 += 1;
            }
            ix0 -= t;
            if ix1 < t1 {
                ix0 -= 1;
            }
            ix1 -= t1;
            q1 += r;
        }
        ix0 = ix0 + ix0 + ((ix1 & sign) >> 31);
        ix1 = ix1 + ix1;
        r >>= 1;
    }

    if (ix0 | ix1) != 0 {
        let mut z = ONE - TINY;
        if z >= ONE {
            z = ONE + TINY;
            if q1 == 0xffff_ffff {
                q1 = 0;
                q += 1;
            } else if z > ONE {
                if q1 == 0xffff_fffe {
                    q += 1;
                }
                q1 += 2;
            } else {
                q1 += q1 & 1;
            }
        }
    }
    ix0 = (q >> 1) + 0x3fe0_0000;
    ix1 = q1 >> 1;
    if (q & 1) != 0 {
        ix1 |= sign;
    }
    ix0 = (ix0 as i32 + (m << 20)) as u32;
    float64_from_u32(ix0, ix1)
}

pub fn js_lrint(a: f64) -> i32 {
    sf64::cvt_sf64_i32(a.to_bits(), RoundingMode::Rne)
}

pub fn js_fmod(a: f64, b: f64) -> f64 {
    f64::from_bits(fmod_sf64(a.to_bits(), b.to_bits()))
}

pub fn js_scalbn(x: f64, n: i32) -> f64 {
    const TWO54: f64 = 1.801_439_850_948_198_4e16;
    const TWOM54: f64 = 5.551_115_123_125_783e-17;

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

const S1: f64 = -1.66666666666666324348e-01;
const S_TAB: [f64; 5] = [
    8.33333333332248946124e-03,
    -1.98412698298579493134e-04,
    2.75573137070700676789e-06,
    -2.50507602534068634195e-08,
    1.58969099521155010221e-10,
];

fn kernel_sin(x: f64, y: f64, iy: i32) -> f64 {
    let ix = (get_high_word(x) & 0x7fff_ffff) as i32;
    if ix < 0x3e40_0000 && (x as i32) == 0 {
        return x;
    }
    let z = x * x;
    let v = z * x;
    let r = eval_poly(z, &S_TAB);
    if iy == 0 {
        x + v * (S1 + z * r)
    } else {
        x - ((z * (HALF * y - v * r) - y) - v * S1)
    }
}

const C_TAB: [f64; 6] = [
    4.16666666666666019037e-02,
    -1.38888888888741095749e-03,
    2.48015872894767294178e-05,
    -2.75573143513906633035e-07,
    2.08757232129817482790e-09,
    -1.13596475577881948265e-11,
];

fn kernel_cos(x: f64, y: f64) -> f64 {
    let ix = (get_high_word(x) & 0x7fff_ffff) as i32;
    if ix < 0x3e40_0000 && (x as i32) == 0 {
        return ONE;
    }
    let z = x * x;
    let r = z * eval_poly(z, &C_TAB);
    if ix < 0x3fd3_3333 {
        ONE - (0.5 * z - (z * r - x * y))
    } else {
        let qx = if ix > 0x3fe9_0000 {
            0.28125
        } else {
            float64_from_u32((ix as u32).wrapping_sub(0x0020_0000), 0)
        };
        let hz = 0.5 * z - qx;
        let a = ONE - qx;
        a - (hz - (z * r - x * y))
    }
}

const REM_PIO2_T_LEN: usize = 19;
const REM_PIO2_T: [u64; REM_PIO2_T_LEN] = [
    0x1580_cc11_bf1e_daea,
    0x9afe_d7ec_47e3_5742,
    0xcf41_ce7d_e294_a4ba,
    0x5d49_eeb1_faf9_7c5e,
    0xd3d1_8fd9_a797_fa8b,
    0xdb4d_9fb3_c9f2_c26d,
    0xfbcb_c462_d682_9b47,
    0xc7fe_25ff_f781_6603,
    0x2721_17e2_ef7e_4a0e,
    0x4e64_758e_60d4_ce7d,
    0x3a67_1c09_ad17_df90,
    0xba20_8d7d_4bae_d121,
    0x3f87_7ac7_2c4a_69cf,
    0x0192_4bba_8274_6487,
    0x6dc9_1b8e_9093_74b8,
    0x7f94_58ea_f7ae_f158,
    0x36d8_a566_4f10_e410,
    0x7f09_d5f4_7d4d_3770,
    0x28be_60db_9391_054a,
];

const PIO4: [u64; 2] = [0xc4c6_628b_80dc_1cd1, 0xc90f_daa2_2168_c234];

fn mul_u64(low: &mut u64, a: u64, b: u64) -> u64 {
    let prod = (a as u128) * (b as u128);
    *low = prod as u64;
    (prod >> 64) as u64
}

fn get_u64_at_bit(tab: &[u64], pos: u32) -> u64 {
    let p = (pos / 64) as usize;
    let shift = pos % 64;
    let mut v = tab[p] >> shift;
    if shift != 0 && (p + 1) < tab.len() {
        v |= tab[p + 1] << (64 - shift);
    }
    v
}

fn rem_pio2_large(x: f64, y: &mut [f64; 2]) -> i32 {
    let mut m = x.to_bits();
    let sgn = (m >> 63) as i32;
    let mut e = ((m >> 52) & 0x7ff) as i32;
    m = (m & ((1u64 << 52) - 1)) | (1u64 << 52);

    let j = (REM_PIO2_T_LEN as i32) * 64 - (e - 1075) - 192;
    let mut d = [0u64; 3];
    for (i, slot) in d.iter_mut().enumerate() {
        *slot = get_u64_at_bit(&REM_PIO2_T, (j + (i as i32) * 64) as u32);
    }
    let mut r0 = 0u64;
    let mut r1 = mul_u64(&mut r0, m, d[0]);
    let mut c = [0u64; 2];
    c[0] = r1;
    r1 = mul_u64(&mut r0, m, d[1]);
    c[0] = c[0].wrapping_add(r0);
    let carry = c[0] < r0;
    c[1] = r1.wrapping_add(if carry { 1 } else { 0 });
    mul_u64(&mut r0, m, d[2]);
    c[1] = c[1].wrapping_add(r0);

    let mut n = (c[1] >> 62) as i32;
    let rnd = ((c[1] >> 61) & 1) as i32;
    n += rnd;
    c[1] = (c[1] << 2) | (c[0] >> 62);
    c[0] <<= 2;
    let mut y_sgn = sgn;
    if rnd != 0 {
        y_sgn ^= 1;
        c[0] = !c[0];
        c[1] = !c[1];
        c[0] = c[0].wrapping_add(1);
        if c[0] == 0 {
            c[1] = c[1].wrapping_add(1);
        }
    }

    r1 = mul_u64(&mut r0, c[0], PIO4[1]);
    d[0] = r0;
    d[1] = r1;

    r1 = mul_u64(&mut r0, c[1], PIO4[0]);
    d[0] = d[0].wrapping_add(r0);
    let mut carry = d[0] < r0;
    d[1] = d[1].wrapping_add(r1);
    let mut carry1 = d[1] < r1;
    d[1] = d[1].wrapping_add(if carry { 1 } else { 0 });
    if d[1] < (if carry { 1 } else { 0 }) {
        carry1 = true;
    }
    d[2] = if carry1 { 1 } else { 0 };

    r1 = mul_u64(&mut r0, c[1], PIO4[1]);
    d[1] = d[1].wrapping_add(r0);
    carry = d[1] < r0;
    d[2] = d[2].wrapping_add(r1).wrapping_add(if carry { 1 } else { 0 });

    if d[2] == 0 {
        y[0] = 0.0;
        y[1] = 0.0;
    } else {
        e = d[2].leading_zeros() as i32;
        if e != 0 {
            d[2] = (d[2] << e) | (d[1] >> (64 - e));
            d[1] <<= e;
        }
        let m0 = (d[2] >> 11) & ((1u64 << 52) - 1);
        let m1 = ((d[2] & 0x7ff) << 42) | (d[1] >> (64 - 42));
        y[0] = f64::from_bits(
            ((y_sgn as u64) << 63) | ((1023 - e) as u64) << 52 | m0,
        );
        if m1 == 0 {
            y[1] = 0.0;
        } else {
            let e1 = m1.leading_zeros() as i32 - 11;
            let m1 = (m1 << e1) & ((1u64 << 52) - 1);
            y[1] = f64::from_bits(
                ((y_sgn as u64) << 63) | ((1023 - e - 53 - e1) as u64) << 52 | m1,
            );
        }
    }
    if sgn != 0 {
        n = -n;
    }
    n
}

const INVPIO2: f64 = 6.36619772367581382433e-01;
const PIO2_TAB: [f64; 3] = [
    1.57079632673412561417e+00,
    6.07710050630396597660e-11,
    2.02226624871116645580e-21,
];
const PIO2_T_TAB: [f64; 3] = [
    6.07710050650619224932e-11,
    2.02226624879595063154e-21,
    8.47842766036889956997e-32,
];
const REM_PIO2_EMAX: [i32; 2] = [16, 49];

pub fn js_rem_pio2(x: f64, y: &mut [f64; 2]) -> i32 {
    let hx = get_high_word(x) as i32;
    let ix = (hx & 0x7fff_ffff) as u32;
    if ix <= 0x3fe9_21fb {
        y[0] = x;
        y[1] = 0.0;
        return 0;
    }
    if ix <= 0x4139_21fb {
        let mut t = x.abs();
        let n: i32;
        let fn_: f64;
        if ix < 0x4002_d97c {
            n = 1;
            fn_ = 1.0;
        } else {
            n = (t * INVPIO2 + HALF) as i32;
            fn_ = n as f64;
        }

        let mut it = 0usize;
        let mut r;
        let mut w;
        loop {
            r = t - fn_ * PIO2_TAB[it];
            w = fn_ * PIO2_T_TAB[it];
            y[0] = r - w;
            let j = (ix >> 20) as i32;
            let i = j - (((get_high_word(y[0]) >> 20) & 0x7ff) as i32);
            if it == 2 || i <= REM_PIO2_EMAX[it] {
                break;
            }
            t = r;
            it += 1;
        }
        y[1] = (r - y[0]) - w;
        if hx < 0 {
            y[0] = -y[0];
            y[1] = -y[1];
            return -n;
        }
        return n;
    }
    if ix >= 0x7ff0_0000 {
        let nan = nan_from(x);
        y[0] = nan;
        y[1] = nan;
        return 0;
    }
    rem_pio2_large(x, y)
}

fn js_sin_cos(x: f64, flag: i32) -> f64 {
    let ix = get_high_word(x) & 0x7fff_ffff;
    if ix >= 0x7ff0_0000 {
        return nan_from(x);
    }

    let mut y = [0.0; 2];
    let n = js_rem_pio2(x, &mut y);
    let mut s = 0.0;
    let mut c = 0.0;
    if flag == 3 || (n & 1) == flag {
        s = kernel_sin(y[0], y[1], 1);
        if flag != 3 {
            if ((n + flag) & 2) != 0 {
                s = -s;
            }
            return s;
        }
    }
    if flag == 3 || (n & 1) != flag {
        c = kernel_cos(y[0], y[1]);
        if flag != 3 {
            s = c;
            if ((n + flag) & 2) != 0 {
                s = -s;
            }
            return s;
        }
    }
    if (n & 1) != 0 {
        -c / s
    } else {
        s / c
    }
}

pub fn js_sin(x: f64) -> f64 {
    js_sin_cos(x, 0)
}

pub fn js_cos(x: f64) -> f64 {
    js_sin_cos(x, 1)
}

pub fn js_tan(x: f64) -> f64 {
    js_sin_cos(x, 3)
}

const PIO2_HI: f64 = 1.57079632679489655800e+00;
const PIO2_LO: f64 = 6.12323399573676603587e-17;
const PIO4_HI: f64 = 7.85398163397448278999e-01;
const P_S: [f64; 6] = [
    1.66666666666666657415e-01,
    -3.25565818622400915405e-01,
    2.01212532134862925881e-01,
    -4.00555345006794114027e-02,
    7.91534994289814532176e-04,
    3.47933107596021167570e-05,
];
const Q_S: [f64; 4] = [
    -2.40339491173441421878e+00,
    2.02094576023350569471e+00,
    -6.88283971605453293030e-01,
    7.70381505559019352791e-02,
];

fn asin_r(t: f64) -> f64 {
    let p = t * eval_poly(t, &P_S);
    let q = ONE + t * eval_poly(t, &Q_S);
    p / q
}

pub fn js_asin(x: f64) -> f64 {
    let hx = get_high_word(x) as i32;
    let ix = (hx & 0x7fff_ffff) as u32;
    if ix >= 0x3ff0_0000 {
        if (ix.wrapping_sub(0x3ff0_0000) | get_low_word(x)) == 0 {
            return x * PIO2_HI + x * PIO2_LO;
        }
        return nan_from_div(x);
    } else if ix < 0x3fe0_0000 {
        if ix < 0x3e40_0000 {
            if HUGE + x > ONE {
                return x;
            }
        } else {
            let t = x * x;
            let w = asin_r(t);
            return x + x * w;
        }
    }
    let w = ONE - x.abs();
    let t = w * 0.5;
    let r = asin_r(t);
    let s = js_sqrt(t);
    let t = if ix >= 0x3fef_3333 {
        PIO2_HI - (2.0 * (s + s * r) - PIO2_LO)
    } else {
        let w = zero_low(s);
        let c = (t - w * w) / (s + w);
        let p = 2.0 * s * r - (PIO2_LO - 2.0 * c);
        let q = PIO4_HI - 2.0 * w;
        PIO4_HI - (p - q)
    };
    if hx > 0 {
        t
    } else {
        -t
    }
}

const PI: f64 = 3.14159265358979311600e+00;

pub fn js_acos(x: f64) -> f64 {
    let hx = get_high_word(x) as i32;
    let ix = (hx & 0x7fff_ffff) as u32;
    if ix >= 0x3ff0_0000 {
        if (ix.wrapping_sub(0x3ff0_0000) | get_low_word(x)) == 0 {
            if hx > 0 {
                return 0.0;
            } else {
                return PI + 2.0 * PIO2_LO;
            }
        }
        return nan_from_div(x);
    }
    if ix < 0x3fe0_0000 {
        if ix <= 0x3c60_0000 {
            return PIO2_HI + PIO2_LO;
        }
        let z = x * x;
        let r = asin_r(z);
        return PIO2_HI - (x - (PIO2_LO - x * r));
    }
    let z = (ONE - x.abs()) * 0.5;
    let r = asin_r(z);
    let s = js_sqrt(z);
    if hx < 0 {
        let w = r * s - PIO2_LO;
        PI - 2.0 * (s + w)
    } else {
        let df = zero_low(s);
        let c = (z - df * df) / (s + df);
        let w = r * s + c;
        2.0 * (df + w)
    }
}

const ATANHI: [f64; 4] = [
    4.63647609000806093515e-01,
    7.85398163397448278999e-01,
    9.82793723247329054082e-01,
    1.57079632679489655800e+00,
];
const ATANLO: [f64; 4] = [
    2.26987774529616870924e-17,
    3.06161699786838301793e-17,
    1.39033110312309984516e-17,
    6.12323399573676603587e-17,
];
const AT_EVEN: [f64; 6] = [
    3.33333333333329318027e-01,
    1.42857142725034663711e-01,
    9.09088713343650656196e-02,
    6.66107313738753120669e-02,
    4.97687799461593236017e-02,
    1.62858201153657823623e-02,
];
const AT_ODD: [f64; 5] = [
    -1.99999999998764832476e-01,
    -1.11111104054623557880e-01,
    -7.69187620504482999495e-02,
    -5.83357013379057348645e-02,
    -3.65315727442169155270e-02,
];

pub fn js_atan(x: f64) -> f64 {
    let hx = get_high_word(x) as i32;
    let ix = (hx & 0x7fff_ffff) as u32;
    if ix >= 0x4410_0000 {
        if ix > 0x7ff0_0000 || (ix == 0x7ff0_0000 && get_low_word(x) != 0) {
            return x + x;
        }
        if hx > 0 {
            return ATANHI[3] + ATANLO[3];
        } else {
            return -ATANHI[3] - ATANLO[3];
        }
    }
    let mut id: i32 = -1;
    let mut x = x;
    if ix < 0x3fdc_0000 {
        if ix < 0x3e20_0000 && HUGE + x > ONE {
            return x;
        }
    } else {
        x = x.abs();
        if ix < 0x3ff3_0000 {
            if ix < 0x3fe6_0000 {
                id = 0;
                x = (2.0 * x - ONE) / (2.0 + x);
            } else {
                id = 1;
                x = (x - ONE) / (x + ONE);
            }
        } else if ix < 0x4003_8000 {
            id = 2;
            x = (x - 1.5) / (ONE + 1.5 * x);
        } else {
            id = 3;
            x = -ONE / x;
        }
    }
    let z = x * x;
    let w = z * z;
    let s1 = z * eval_poly(w, &AT_EVEN);
    let s2 = w * eval_poly(w, &AT_ODD);
    if id < 0 {
        x - x * (s1 + s2)
    } else {
        let z = ATANHI[id as usize] - ((x * (s1 + s2) - ATANLO[id as usize]) - x);
        if hx < 0 {
            -z
        } else {
            z
        }
    }
}

const PI_O_4: f64 = 7.8539816339744827900E-01;
const PI_O_2: f64 = 1.5707963267948965580E+00;
const PI_LO: f64 = 1.2246467991473531772E-16;

pub fn js_atan2(y: f64, x: f64) -> f64 {
    let (hx_u, lx) = extract_words(x);
    let (hy_u, ly) = extract_words(y);
    let hx = hx_u as i32;
    let hy = hy_u as i32;
    let ix = hx_u & 0x7fff_ffff;
    let iy = hy_u & 0x7fff_ffff;

    if (ix | ((lx | lx.wrapping_neg()) >> 31)) > 0x7ff0_0000
        || (iy | ((ly | ly.wrapping_neg()) >> 31)) > 0x7ff0_0000
    {
        return x + y;
    }
    if (hx_u.wrapping_sub(0x3ff0_0000) | lx) == 0 {
        return js_atan(y);
    }
    let m = ((hy >> 31) & 1) | ((hx >> 30) & 2);

    let mut z;
    if (iy | ly) == 0 {
        z = 0.0;
    } else if (ix | lx) == 0 {
        return if hy < 0 { -PI_O_2 - TINY } else { PI_O_2 + TINY };
    } else if ix == 0x7ff0_0000 {
        if iy == 0x7ff0_0000 {
            z = PI_O_4;
        } else {
            z = 0.0;
        }
    } else if iy == 0x7ff0_0000 {
        return if hy < 0 { -PI_O_2 - TINY } else { PI_O_2 + TINY };
    } else {
        let k = (iy as i32 - ix as i32) >> 20;
        if k > 60 {
            z = PI_O_2 + 0.5 * PI_LO;
        } else if hx < 0 && k < -60 {
            z = 0.0;
        } else {
            z = js_atan((y / x).abs());
        }
    }

    match m {
        0 => z,
        1 => {
            z = set_high_word(z, get_high_word(z) ^ 0x8000_0000);
            z
        }
        2 => PI - (z - PI_LO),
        _ => (z - PI_LO) - PI,
    }
}

const TWO: f64 = 2.0;
const HALF_TAB: [f64; 2] = [0.5, -0.5];
const TWOM1000: f64 = 9.33263618503218878990e-302;
const O_THRESHOLD: f64 = 7.09782712893383973096e+02;
const U_THRESHOLD: f64 = -7.45133219101941108420e+02;
const LN2_HI: [f64; 2] = [
    6.93147180369123816490e-01,
    -6.93147180369123816490e-01,
];
const LN2_LO: [f64; 2] = [
    1.90821492927058770002e-10,
    -1.90821492927058770002e-10,
];
const INV_LN2: f64 = 1.44269504088896338700e+00;
const EXP_P: [f64; 5] = [
    1.66666666666666019037e-01,
    -2.77777777770155933842e-03,
    6.61375632143793436117e-05,
    -1.65339022054652515390e-06,
    4.13813679705723846039e-08,
];

fn kernel_exp(z: f64, w: f64, lo: f64, hi: f64, n: i32) -> f64 {
    let t = z * z;
    let t1 = z - t * eval_poly(t, &EXP_P);
    let r = (z * t1) / (t1 - TWO) - (w + z * w);
    let mut z = ONE - ((lo + r) - hi);
    let mut j = get_high_word(z) as i32;
    j = j.wrapping_add(n << 20);
    if (j >> 20) <= 0 {
        z = js_scalbn(z, n);
    } else {
        z = set_high_word(z, (get_high_word(z) as i32).wrapping_add(n << 20) as u32);
    }
    z
}

pub fn js_exp(x: f64) -> f64 {
    let mut hi = 0.0;
    let mut lo = 0.0;
    let mut k = 0;
    let mut x = x;
    let mut hx = get_high_word(x);
    let xsb = ((hx >> 31) & 1) as usize;
    hx &= 0x7fff_ffff;

    if hx >= 0x4086_2e42 {
        if hx >= 0x7ff0_0000 {
            if ((hx & 0x000f_ffff) | get_low_word(x)) != 0 {
                return x + x;
            } else if xsb == 0 {
                return x;
            } else {
                return 0.0;
            }
        }
        if x > O_THRESHOLD {
            return HUGE * HUGE;
        }
        if x < U_THRESHOLD {
            return TWOM1000 * TWOM1000;
        }
    }

    if hx > 0x3fd6_2e42 {
        if hx < 0x3ff0_a2b2 {
            hi = x - LN2_HI[xsb];
            lo = LN2_LO[xsb];
            k = 1 - (xsb as i32) - (xsb as i32);
        } else {
            k = (INV_LN2 * x + HALF_TAB[xsb]) as i32;
            let t = k as f64;
            hi = x - t * LN2_HI[0];
            lo = t * LN2_LO[0];
        }
        x = hi - lo;
    } else if hx < 0x3e30_0000 && HUGE + x > ONE {
        return ONE + x;
    }

    if k == 0 {
        lo = 0.0;
        hi = x;
    }
    kernel_exp(x, 0.0, lo, hi, k)
}

const BP: [f64; 2] = [1.0, 1.5];
const DP_H: [f64; 2] = [0.0, 5.84962487220764160156e-01];
const DP_L: [f64; 2] = [0.0, 1.35003920212974897128e-08];
const TWO53: f64 = 9_007_199_254_740_992.0;
const LG2: f64 = 6.93147180559945286227e-01;
const LG2_H: f64 = 6.93147182464599609375e-01;
const LG2_L: f64 = -1.90465429995776804525e-09;
const OVT: f64 = 8.0085662595372944372e-17;
const CP: f64 = 9.61796693925975554329e-01;
const CP_H: f64 = 9.61796700954437255859e-01;
const CP_L: f64 = -7.02846165095275826516e-09;
const IVLN2: f64 = 1.44269504088896338700e+00;
const IVLN2_H: f64 = 1.44269502162933349609e+00;
const IVLN2_L: f64 = 1.92596299112661746887e-08;
const IVLG10B2: f64 = 0.3010299956639812;
const IVLG10B2_H: f64 = 0.30102992057800293;
const IVLG10B2_L: f64 = 7.508597826552624e-8;
const L_TAB: [f64; 6] = [
    5.99999999999994648725e-01,
    4.28571428578550184252e-01,
    3.33333329818377432918e-01,
    2.72728123808534006489e-01,
    2.30660745775561754067e-01,
    2.06975017800338417784e-01,
];

fn kernel_log2(pt1: &mut f64, pt2: &mut f64, mut ax: f64) {
    let mut n: i32 = 0;
    let mut ix = get_high_word(ax);
    if ix < 0x0010_0000 {
        ax *= TWO53;
        n -= 53;
        ix = get_high_word(ax);
    }
    n += ((ix >> 20) as i32) - 0x3ff;
    let j = ix & 0x000f_ffff;
    ix = j | 0x3ff0_0000;
    let k;
    if j <= 0x3988e {
        k = 0;
    } else if j < 0x000b_b67a {
        k = 1;
    } else {
        k = 0;
        n += 1;
        ix = ix.wrapping_sub(0x0010_0000);
    }
    ax = set_high_word(ax, ix);

    let u = ax - BP[k];
    let v = ONE / (ax + BP[k]);
    let ss = u * v;
    let s_h = zero_low(ss);
    let mut t_h = ZERO;
    t_h = set_high_word(t_h, ((ix >> 1) | 0x2000_0000) + 0x0008_0000 + ((k as u32) << 18));
    let t_l = ax - (t_h - BP[k]);
    let s_l = v * ((u - s_h * t_h) - s_h * t_l);
    let s2 = ss * ss;
    let mut r = s2 * s2 * eval_poly(s2, &L_TAB);
    r += s_l * (s_h + ss);
    let s2 = s_h * s_h;
    t_h = zero_low(3.0 + s2 + r);
    let t_l = r - ((t_h - 3.0) - s2);
    let u = s_h * t_h;
    let v = s_l * t_h + t_l * ss;
    let p_h = zero_low(u + v);
    let p_l = v - (p_h - u);
    let z_h = CP_H * p_h;
    let z_l = CP_L * p_h + p_l * CP + DP_L[k];
    let t = n as f64;
    let t1 = zero_low((z_h + z_l + DP_H[k]) + t);
    let t2 = z_l - (((t1 - t) - DP_H[k]) - z_h);
    *pt1 = t1;
    *pt2 = t2;
}

fn js_log_internal(x: f64, flag: i32) -> f64 {
    let (hx_u, lx) = extract_words(x);
    let hx = hx_u as i32;
    if hx <= 0 {
        if ((hx_u & 0x7fff_ffff) | lx) == 0 {
            return f64::NEG_INFINITY;
        }
        if hx < 0 {
            return f64::NAN;
        }
    } else if hx_u >= 0x7ff0_0000 {
        return x + x;
    }
    let mut p_h = 0.0;
    let mut p_l = 0.0;
    kernel_log2(&mut p_h, &mut p_l, x);

    let mut t = p_h + p_l;
    if flag == 0 {
        return t;
    }
    t = zero_low(t);
    if flag == 1 {
        let u = t * LG2_H;
        let v = (p_l - (t - p_h)) * LG2 + t * LG2_L;
        u + v
    } else {
        let u = t * IVLG10B2_H;
        let v = (p_l - (t - p_h)) * IVLG10B2 + t * IVLG10B2_L;
        u + v
    }
}

pub fn js_log2(x: f64) -> f64 {
    js_log_internal(x, 0)
}

pub fn js_log(x: f64) -> f64 {
    js_log_internal(x, 1)
}

pub fn js_log10(x: f64) -> f64 {
    js_log_internal(x, 2)
}

pub fn js_pow(x: f64, y: f64) -> f64 {
    let (hx_u, lx) = extract_words(x);
    let (hy_u, ly) = extract_words(y);
    let hx = hx_u as i32;
    let hy = hy_u as i32;
    let ix = hx_u & 0x7fff_ffff;
    let iy = hy_u & 0x7fff_ffff;

    if (iy | ly) == 0 {
        return ONE;
    }
    if ix > 0x7ff0_0000
        || (ix == 0x7ff0_0000 && lx != 0)
        || iy > 0x7ff0_0000
        || (iy == 0x7ff0_0000 && ly != 0)
    {
        return x + y;
    }

    let mut yisint = 0;
    if hx < 0 {
        if iy >= 0x4340_0000 {
            yisint = 2;
        } else if iy >= 0x3ff0_0000 {
            let k = ((iy >> 20) as i32) - 0x3ff;
            if k > 20 {
                let shift = (52 - k) as u32;
                let j = ly >> shift;
                if (j << shift) == ly {
                    yisint = 2 - (j & 1) as i32;
                }
            } else if ly == 0 {
                let shift = (20 - k) as u32;
                let j = iy >> shift;
                if (j << shift) == iy {
                    yisint = 2 - (j & 1) as i32;
                }
            }
        }
    }

    if ly == 0 {
        if iy == 0x7ff0_0000 {
            if (ix.wrapping_sub(0x3ff0_0000) | lx) == 0 {
                return nan_from(y);
            } else if ix >= 0x3ff0_0000 {
                return if hy >= 0 { y } else { ZERO };
            } else {
                return if hy < 0 { -y } else { ZERO };
            }
        }
        if iy == 0x3ff0_0000 {
            return if hy < 0 { ONE / x } else { x };
        }
        if hy_u == 0x4000_0000 {
            return x * x;
        }
        if hy_u == 0x3fe0_0000 && hx >= 0 {
            return js_sqrt(x);
        }
    }

    let ax = x.abs();
    if lx == 0 && (ix == 0x7ff0_0000 || ix == 0 || ix == 0x3ff0_0000) {
        let mut z = ax;
        if hy < 0 {
            z = ONE / z;
        }
        if hx < 0 {
            if (ix.wrapping_sub(0x3ff0_0000) | (yisint as u32)) == 0 {
                z = nan_from_div(z);
            } else if yisint == 1 {
                z = -z;
            }
        }
        return z;
    }

    let n = (hx >> 31) + 1;
    if (n | yisint) == 0 {
        return nan_from_div(x);
    }
    let mut s = ONE;
    if (n | (yisint - 1)) == 0 {
        s = -ONE;
    }

    let t1;
    let t2;
    if iy > 0x41e0_0000 {
        if iy > 0x43f0_0000 {
            if ix <= 0x3fef_ffff {
                return if hy < 0 { HUGE * HUGE } else { TINY * TINY };
            }
            if ix >= 0x3ff0_0000 {
                return if hy > 0 { HUGE * HUGE } else { TINY * TINY };
            }
        }
        if ix < 0x3fef_ffff {
            return if hy < 0 {
                s * HUGE * HUGE
            } else {
                s * TINY * TINY
            };
        }
        if ix > 0x3ff0_0000 {
            return if hy > 0 {
                s * HUGE * HUGE
            } else {
                s * TINY * TINY
            };
        }
        let t = ax - ONE;
        let w = (t * t) * (0.5 - t * (0.333_333_333_333_333_3 - t * 0.25));
        let u = IVLN2_H * t;
        let v = t * IVLN2_L - w * IVLN2;
        t1 = zero_low(u + v);
        t2 = v - (t1 - u);
    } else {
        let mut p_h = 0.0;
        let mut p_l = 0.0;
        kernel_log2(&mut p_h, &mut p_l, ax);
        t1 = p_h;
        t2 = p_l;
    }

    let y1 = zero_low(y);
    let p_l = (y - y1) * t1 + y * t2;
    let mut p_h = y1 * t1;
    let mut z = p_l + p_h;
    let (j_u, i_u) = extract_words(z);
    let j = j_u as i32;
    let i = i_u as i32;
    if j >= 0x4090_0000 {
        if ((j.wrapping_sub(0x4090_0000)) | i) != 0 {
            return s * HUGE * HUGE;
        }
        if p_l + OVT > z - p_h {
            return s * HUGE * HUGE;
        }
    } else if (j & 0x7fff_ffff) >= 0x4090_cc00 {
        if ((j.wrapping_sub(0xc090_cc00u32 as i32)) | i) != 0 {
            return s * TINY * TINY;
        }
        if p_l <= z - p_h {
            return s * TINY * TINY;
        }
    }

    let i = j & 0x7fff_ffff;
    let mut k = (i >> 20) - 0x3ff;
    let mut n = 0;
    if i > 0x3fe0_0000 {
        n = j + (0x0010_0000 >> (k + 1));
        k = ((n & 0x7fff_ffff) >> 20) - 0x3ff;
        let mut t = ZERO;
        t = set_high_word(t, (n as u32) & !(0x000f_ffff >> k));
        n = ((n & 0x000f_ffff) | 0x0010_0000) >> (20 - k);
        if j < 0 {
            n = -n;
        }
        p_h -= t;
    }
    let t = zero_low(p_l + p_h);
    let u = t * LG2_H;
    let v = (p_l - (t - p_h)) * LG2 + t * LG2_L;
    z = u + v;
    let w = v - (z - u);
    s * kernel_exp(z, w, 0.0, z, n)
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

    #[test]
    fn fmod_handles_edge_cases() {
        assert_eq!(js_fmod(5.5, 2.0), 1.5);
        assert_eq!(js_fmod(-5.5, 2.0), -1.5);
        assert_eq!(js_fmod(1.25, f64::INFINITY), 1.25);

        let res = js_fmod(-4.0, 2.0);
        assert_eq!(res.to_bits(), (-0.0f64).to_bits());

        let res = js_fmod(-1.25, 2.0);
        assert_eq!(res.to_bits(), (-1.25f64).to_bits());

        assert!(js_fmod(f64::INFINITY, 2.0).is_nan());
        assert!(js_fmod(1.0, 0.0).is_nan());
        assert!(js_fmod(1.0, f64::NAN).is_nan());
    }

    #[test]
    fn trig_basic_values() {
        assert_eq!(js_sin(0.0), 0.0);
        assert_eq!(js_cos(0.0), 1.0);
        let neg_zero = js_tan(-0.0);
        assert_eq!(neg_zero.to_bits(), (-0.0f64).to_bits());
        assert!(js_sin(f64::INFINITY).is_nan());
        assert!(js_cos(f64::NAN).is_nan());
    }

    #[test]
    fn rem_pio2_small_inputs() {
        let mut y = [0.0; 2];
        let n = js_rem_pio2(0.1, &mut y);
        assert_eq!(n, 0);
        assert_eq!(y[0], 0.1);
        assert_eq!(y[1], 0.0);
    }

    #[test]
    fn asin_acos_domains() {
        assert_eq!(js_asin(0.0), 0.0);
        assert_eq!(js_acos(1.0), 0.0);
        assert!(js_asin(2.0).is_nan());
        assert!(js_acos(2.0).is_nan());
    }

    #[test]
    fn atan2_quadrant_signs() {
        assert_eq!(js_atan2(0.0, -1.0), PI + PI_LO);
        assert_eq!(js_atan2(-0.0, -1.0), -PI - PI_LO);
    }

    #[test]
    fn exp_log_basic_values() {
        assert_eq!(js_exp(0.0), 1.0);
        assert_eq!(js_log(1.0), 0.0);
        assert_eq!(js_log2(1.0), 0.0);
        assert_eq!(js_log10(1.0), 0.0);
        assert_eq!(js_log(0.0), f64::NEG_INFINITY);
        assert!(js_log(-1.0).is_nan());
    }

    #[test]
    fn pow_basic_cases() {
        assert_eq!(js_pow(2.0, 3.0), 8.0);
        assert_eq!(js_pow(-2.0, 3.0), -8.0);
        assert_eq!(js_pow(-2.0, 4.0), 16.0);
        assert!(js_pow(-2.0, 0.5).is_nan());
        assert_eq!(js_pow(2.0, 0.0), 1.0);
    }
}
