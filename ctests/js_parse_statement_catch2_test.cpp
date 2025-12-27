#include <catch2/catch_test_macros.hpp>

#include <cstdlib>
#include <cstring>
#include <string>

extern "C" {
#include "mquickjs.h"
extern const JSSTDLibraryDef js_stdlib;
}

namespace {

struct TestContext {
    void *mem = nullptr;
    JSContext *ctx = nullptr;
};

TestContext make_context()
{
    constexpr size_t kMemSize = 4 * 1024 * 1024;
    TestContext env;
    env.mem = std::malloc(kMemSize);
    REQUIRE(env.mem != nullptr);
    env.ctx = JS_NewContext(env.mem, kMemSize, &js_stdlib);
    REQUIRE(env.ctx != nullptr);
    return env;
}

void free_context(TestContext &env)
{
    if (env.ctx) {
        JS_FreeContext(env.ctx);
        env.ctx = nullptr;
    }
    if (env.mem) {
        std::free(env.mem);
        env.mem = nullptr;
    }
}

JSValue eval_expr(JSContext *ctx, const char *source)
{
    JSValue val = JS_Eval(ctx, source, std::strlen(source), "<test>", JS_EVAL_RETVAL);
    REQUIRE(!JS_IsException(val));
    return val;
}

int to_int(JSContext *ctx, JSValue val)
{
    int out = 0;
    REQUIRE(JS_ToInt32(ctx, &out, val) == 0);
    return out;
}

std::string to_string(JSContext *ctx, JSValue val)
{
    JSCStringBuf buf;
    size_t len = 0;
    const char *str = JS_ToCStringLen(ctx, &len, val, &buf);
    REQUIRE(str != nullptr);
    return std::string(str, len);
}

} // namespace

TEST_CASE("js_parse_statement handles labelled blocks and break", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var out = 0;"
                            "  label: { out = 1; break label; out = 2; }"
                            "  return out;"
                            "})()");
    CHECK(to_int(ctx, res) == 1);

    free_context(env);
}

TEST_CASE("js_parse_statement parses while and continue", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var i = 0;"
                            "  var sum = 0;"
                            "  while (i < 5) {"
                            "    i++;"
                            "    if (i % 2 == 0) continue;"
                            "    sum += i;"
                            "  }"
                            "  return sum;"
                            "})()");
    CHECK(to_int(ctx, res) == 9);

    free_context(env);
}

TEST_CASE("js_parse_statement parses do while loops", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var i = 0;"
                            "  do { i++; } while (i < 3);"
                            "  return i;"
                            "})()");
    CHECK(to_int(ctx, res) == 3);

    free_context(env);
}

TEST_CASE("js_parse_statement parses for and for-in", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var sum = 0;"
                            "  for (var i = 0; i < 4; i++) { sum += i; }"
                            "  var obj = { a: 1, b: 2 };"
                            "  var count = 0;"
                            "  for (var k in obj) { count++; }"
                            "  return sum * 10 + count;"
                            "})()");
    CHECK(to_int(ctx, res) == 62);

    free_context(env);
}

TEST_CASE("js_parse_statement parses switch statements", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var out = 0;"
                            "  switch (2) {"
                            "    case 1: out = 1; break;"
                            "    case 2: out = 2; break;"
                            "    default: out = 3;"
                            "  }"
                            "  return out;"
                            "})()");
    CHECK(to_int(ctx, res) == 2);

    free_context(env);
}

TEST_CASE("js_parse_statement parses try catch finally", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var log = '';"
                            "  try { throw 1; }"
                            "  catch (e) { log = 'c'; }"
                            "  finally { log += 'f'; }"
                            "  return log;"
                            "})()");
    CHECK(to_string(ctx, res) == "cf");

    free_context(env);
}

TEST_CASE("js_parse_statement rejects throw with line terminator", "[js_parse_statement]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    const char *source = "throw\n1";
    JSValue val = JS_Eval(ctx, source, std::strlen(source), "<test>", 0);
    CHECK(JS_IsException(val));

    free_context(env);
}
