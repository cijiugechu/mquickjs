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

JSValue get_prop(JSContext *ctx, JSValue obj, const char *name)
{
    JSValue val = JS_GetPropertyStr(ctx, obj, name);
    REQUIRE(!JS_IsException(val));
    return val;
}

JSValue get_elem(JSContext *ctx, JSValue obj, uint32_t idx)
{
    JSValue val = JS_GetPropertyUint32(ctx, obj, idx);
    REQUIRE(!JS_IsException(val));
    return val;
}

std::string to_string(JSContext *ctx, JSValue val)
{
    JSCStringBuf buf;
    size_t len = 0;
    const char *str = JS_ToCStringLen(ctx, &len, val, &buf);
    REQUIRE(str != nullptr);
    return std::string(str, len);
}

int to_int(JSContext *ctx, JSValue val)
{
    int out = 0;
    REQUIRE(JS_ToInt32(ctx, &out, val) == 0);
    return out;
}

bool to_bool(JSValue val)
{
    REQUIRE(JS_IsBool(val));
    return JS_VALUE_GET_SPECIAL_VALUE(val) != 0;
}

} // namespace

TEST_CASE("lre_exec returns captures and index for exec", "[lre_exec]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx, "/a(b)c/.exec('zabc')");
    REQUIRE(!JS_IsNull(res));

    CHECK(to_int(ctx, get_prop(ctx, res, "length")) == 2);
    CHECK(to_string(ctx, get_elem(ctx, res, 0)) == "abc");
    CHECK(to_string(ctx, get_elem(ctx, res, 1)) == "b");
    CHECK(to_int(ctx, get_prop(ctx, res, "index")) == 1);
    CHECK(to_string(ctx, get_prop(ctx, res, "input")) == "zabc");

    free_context(env);
}

TEST_CASE("lre_exec handles case-insensitive backreferences", "[lre_exec]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx, "/([a])\\1/i.exec('aA')");
    REQUIRE(!JS_IsNull(res));
    CHECK(to_string(ctx, get_elem(ctx, res, 0)) == "aA");
    CHECK(to_string(ctx, get_elem(ctx, res, 1)) == "a");

    free_context(env);
}

TEST_CASE("lre_exec supports lookahead assertions", "[lre_exec]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx, "/foo(?=bar)/.exec('foobar')");
    REQUIRE(!JS_IsNull(res));
    CHECK(to_string(ctx, get_elem(ctx, res, 0)) == "foo");
    CHECK(to_int(ctx, get_prop(ctx, res, "index")) == 0);

    CHECK(!to_bool(eval_expr(ctx, "/foo(?!bar)/.test('foobar')")));
    CHECK(to_bool(eval_expr(ctx, "/foo(?!bar)/.test('foox')")));

    free_context(env);
}

TEST_CASE("lre_exec matches word boundaries and ranges", "[lre_exec]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    CHECK(to_bool(eval_expr(ctx, "/\\bcat\\b/.test('a cat!')")));
    CHECK(!to_bool(eval_expr(ctx, "/\\bcat\\b/.test('scatter')")));

    JSValue res = eval_expr(ctx, "/[a-c]+/i.exec('xxCBAyy')");
    REQUIRE(!JS_IsNull(res));
    CHECK(to_string(ctx, get_elem(ctx, res, 0)) == "CBA");

    free_context(env);
}

TEST_CASE("lre_exec honors multiline and dotall flags", "[lre_exec]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx, "/^bar/m.exec('foo\\nbar\\nbaz')");
    REQUIRE(!JS_IsNull(res));
    CHECK(to_string(ctx, get_elem(ctx, res, 0)) == "bar");
    CHECK(to_int(ctx, get_prop(ctx, res, "index")) == 4);

    CHECK(!to_bool(eval_expr(ctx, "/a.b/.test('a\\nb')")));
    JSValue dotall = eval_expr(ctx, "/a.b/s.exec('a\\nb')");
    REQUIRE(!JS_IsNull(dotall));
    CHECK(to_string(ctx, get_elem(ctx, dotall, 0)) == "a\nb");

    free_context(env);
}

TEST_CASE("lre_exec respects sticky lastIndex", "[lre_exec]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = eval_expr(ctx,
                            "(function(){"
                            "  var re=/a/y;"
                            "  re.lastIndex=1;"
                            "  var m=re.exec('ba');"
                            "  return [m && m[0], re.lastIndex];"
                            "})()");
    REQUIRE(!JS_IsNull(res));
    CHECK(to_string(ctx, get_elem(ctx, res, 0)) == "a");
    CHECK(to_int(ctx, get_elem(ctx, res, 1)) == 2);

    JSValue res_fail = eval_expr(ctx,
                                 "(function(){"
                                 "  var re=/a/y;"
                                 "  re.lastIndex=0;"
                                 "  var m=re.exec('ba');"
                                 "  return [m, re.lastIndex];"
                                 "})()");
    REQUIRE(!JS_IsNull(res_fail));
    CHECK(JS_IsNull(get_elem(ctx, res_fail, 0)));
    CHECK(to_int(ctx, get_elem(ctx, res_fail, 1)) == 0);

    free_context(env);
}
