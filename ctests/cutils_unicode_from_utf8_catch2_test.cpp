#include <catch2/catch_test_macros.hpp>

#include <cstddef>
#include <cstdint>

extern "C" {
#include "cutils.h"
}

static void expect_decode(const uint8_t *bytes, size_t max_len, int expected,
                          size_t expected_len)
{
    size_t len = 0;
    int code = __unicode_from_utf8(bytes, max_len, &len);
    CHECK(code == expected);
    CHECK(len == expected_len);
}

TEST_CASE("__unicode_from_utf8 decodes valid sequences", "[cutils]")
{
    const uint8_t two_bytes[] = { 0xC2, 0xA2 };
    const uint8_t three_bytes[] = { 0xE2, 0x82, 0xAC };
    const uint8_t surrogate[] = { 0xED, 0xA0, 0x80 };
    const uint8_t four_bytes[] = { 0xF0, 0x9F, 0x98, 0x80 };

    expect_decode(two_bytes, sizeof(two_bytes), 0x00A2, 2);
    expect_decode(three_bytes, sizeof(three_bytes), 0x20AC, 3);
    expect_decode(surrogate, sizeof(surrogate), 0xD800, 3);
    expect_decode(four_bytes, sizeof(four_bytes), 0x1F600, 4);
}

TEST_CASE("__unicode_from_utf8 rejects invalid sequences", "[cutils]")
{
    const uint8_t ascii[] = { 0x41 };
    const uint8_t cont[] = { 0x80 };
    const uint8_t invalid_two[] = { 0xC2, 0x41 };
    const uint8_t overlong_two[] = { 0xC0, 0x80 };
    const uint8_t truncated_three[] = { 0xE2, 0x82 };
    const uint8_t overlong_three[] = { 0xE0, 0x80, 0x80 };
    const uint8_t truncated_four[] = { 0xF0, 0x9F, 0x98, 0x80 };
    const uint8_t out_of_range[] = { 0xF4, 0x90, 0x80, 0x80 };

    expect_decode(ascii, sizeof(ascii), -1, 1);
    expect_decode(cont, sizeof(cont), -1, 1);
    expect_decode(invalid_two, sizeof(invalid_two), -1, 1);
    expect_decode(overlong_two, sizeof(overlong_two), -1, 2);
    expect_decode(truncated_three, 2, -1, 2);
    expect_decode(overlong_three, sizeof(overlong_three), -1, 3);
    expect_decode(truncated_four, 3, -1, 3);
    expect_decode(out_of_range, sizeof(out_of_range), -1, 4);
}
