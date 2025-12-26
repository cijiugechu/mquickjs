#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

uint32_t dtoa_udiv1norm_init(uint32_t d);
uint32_t dtoa_udiv1norm(uint32_t *r, uint32_t a1, uint32_t a0, uint32_t d,
                        uint32_t d_inv);
uint32_t dtoa_limb_bits(void);

#ifdef __cplusplus
}
#endif
