#include "dtoa_udiv1norm_helpers.h"

/*
 * Pull dtoa.c into this translation unit so we can access static helpers.
 */
#include "../mquickjs-c/dtoa.c"

uint32_t dtoa_udiv1norm_init(uint32_t d)
{
    return udiv1norm_init(d);
}

uint32_t dtoa_udiv1norm(uint32_t *r, uint32_t a1, uint32_t a0, uint32_t d,
                        uint32_t d_inv)
{
    return udiv1norm(r, a1, a0, d, d_inv);
}

uint32_t dtoa_limb_bits(void)
{
    return LIMB_BITS;
}
