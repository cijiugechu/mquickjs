#include <catch2/catch_test_macros.hpp>

#include <cstdint>

#include "dtoa_udiv1norm_helpers.h"

static void check_udiv1norm_case(uint32_t d, uint32_t a1, uint32_t a0)
{
    uint32_t r = 0;
    uint32_t d_inv = dtoa_udiv1norm_init(d);
    uint32_t q = dtoa_udiv1norm(&r, a1, a0, d, d_inv);
    uint32_t limb_bits = dtoa_limb_bits();
    uint64_t numerator = (static_cast<uint64_t>(a1) << limb_bits) | a0;
    uint64_t q_ref = numerator / d;
    uint64_t r_ref = numerator % d;

    CHECK(q == static_cast<uint32_t>(q_ref));
    CHECK(r == static_cast<uint32_t>(r_ref));
}

TEST_CASE("dtoa udiv1norm matches reference division", "[dtoa]")
{
    const uint32_t limb_bits = dtoa_limb_bits();
    REQUIRE(limb_bits >= 2);

    const uint32_t ds[] = {
        static_cast<uint32_t>(1u << (limb_bits - 1)),
        0x80000001u,
        0x90000000u,
        0xffffffffu,
    };
    const uint32_t a0s[] = {
        0u,
        1u,
        0x80000000u,
        0xffffffffu,
        0x12345678u,
    };

    for (uint32_t d : ds) {
        const uint32_t a1s[] = { 0u, 1u, d / 2, d - 1 };
        for (uint32_t a1 : a1s) {
            if (a1 >= d) {
                continue;
            }
            for (uint32_t a0 : a0s) {
                check_udiv1norm_case(d, a1, a0);
            }
        }
    }
}
