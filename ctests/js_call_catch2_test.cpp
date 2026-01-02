#include <catch2/catch_test_macros.hpp>

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <vector>

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

void eval_script(JSContext *ctx, const char *source)
{
    JSValue val = JS_Eval(ctx, source, std::strlen(source), "<test>", 0);
    REQUIRE(!JS_IsException(val));
}

JSValue get_property(JSContext *ctx, JSValue obj, const char *name)
{
    JSValue val = JS_GetPropertyStr(ctx, obj, name);
    REQUIRE(!JS_IsException(val));
    return val;
}

JSValue call_js(JSContext *ctx, JSValue func, JSValue this_obj,
                std::initializer_list<JSValue> args, int extra_flags = 0)
{
    struct GCRefStack {
        JSContext *ctx = nullptr;
        std::vector<JSGCRef> refs;
        size_t count = 0;

        GCRefStack(JSContext *ctx, size_t capacity) : ctx(ctx), refs(capacity) {}

        void push(JSValue val)
        {
            JS_PushGCRef(ctx, &refs[count]);
            refs[count].val = val;
            count++;
        }

        ~GCRefStack()
        {
            while (count > 0) {
                count--;
                JS_PopGCRef(ctx, &refs[count]);
            }
        }
    };

    std::vector<JSValue> argv(args);
    const int argc = static_cast<int>(argv.size());
    GCRefStack roots(ctx, static_cast<size_t>(argc) + 2);
    roots.push(func);
    roots.push(this_obj);
    for (const auto &arg : argv) {
        roots.push(arg);
    }

    REQUIRE(JS_StackCheck(ctx, argc + 2) == 0);
    for (int i = argc - 1; i >= 0; --i) {
        JS_PushArg(ctx, argv[static_cast<size_t>(i)]);
    }
    JS_PushArg(ctx, func);
    JS_PushArg(ctx, this_obj);
    return JS_Call(ctx, extra_flags | argc);
}

} // namespace

TEST_CASE("JS_Call invokes JS bytecode functions", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    eval_script(ctx, "function add(a, b) { return a + b; }");
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue func = get_property(ctx, global, "add");
    REQUIRE(JS_IsFunction(ctx, func));

    JSValue res = call_js(ctx, func, JS_UNDEFINED,
                          {JS_NewInt32(ctx, 2), JS_NewInt32(ctx, 3)});
    REQUIRE(!JS_IsException(res));
    int out = 0;
    REQUIRE(JS_ToInt32(ctx, &out, res) == 0);
    CHECK(out == 5);

    free_context(env);
}

TEST_CASE("JS_Call binds explicit this for methods", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    eval_script(ctx, "var obj = { x: 40, sum: function(a) { return this.x + a; } };");
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue obj = get_property(ctx, global, "obj");
    JSValue sum = get_property(ctx, obj, "sum");
    REQUIRE(JS_IsFunction(ctx, sum));

    JSValue res = call_js(ctx, sum, obj, {JS_NewInt32(ctx, 2)});
    REQUIRE(!JS_IsException(res));
    int out = 0;
    REQUIRE(JS_ToInt32(ctx, &out, res) == 0);
    CHECK(out == 42);

    free_context(env);
}

TEST_CASE("JS_Call fills missing arguments with undefined", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    eval_script(ctx, "function arg2_missing(a, b) { return b === undefined; }");
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue func = get_property(ctx, global, "arg2_missing");
    REQUIRE(JS_IsFunction(ctx, func));

    JSValue res = call_js(ctx, func, JS_UNDEFINED, {JS_NewInt32(ctx, 1)});
    REQUIRE(!JS_IsException(res));
    REQUIRE(JS_IsBool(res));
    CHECK(JS_VALUE_GET_SPECIAL_VALUE(res) == 1);

    free_context(env);
}

TEST_CASE("multiplication preserves negative zero for int operands", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    const char *source = "1 / (0 * -6)";
    JSValue val = JS_Eval(ctx, source, std::strlen(source), "<test>", JS_EVAL_RETVAL);
    REQUIRE(!JS_IsException(val));
    double out = 0.0;
    REQUIRE(JS_ToNumber(ctx, &out, val) == 0);
    CHECK(std::isinf(out));
    CHECK(std::signbit(out));

    free_context(env);
}

TEST_CASE("JS_Call constructs Array with FRAME_CF_CTOR", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue array_ctor = get_property(ctx, global, "Array");
    REQUIRE(JS_IsFunction(ctx, array_ctor));

    JSValue res = call_js(ctx, array_ctor, JS_UNDEFINED,
                          {JS_NewInt32(ctx, 10), JS_NewInt32(ctx, 20)},
                          FRAME_CF_CTOR);
    REQUIRE(!JS_IsException(res));

    JSGCRef res_ref;
    JS_PUSH_VALUE(ctx, res);
    JSValue len_val = get_property(ctx, res, "length");
    int len = 0;
    REQUIRE(JS_ToInt32(ctx, &len, len_val) == 0);
    CHECK(len == 2);
    JSValue idx0 = get_property(ctx, res, "0");
    int v0 = 0;
    REQUIRE(JS_ToInt32(ctx, &v0, idx0) == 0);
    CHECK(v0 == 10);
    JS_POP_VALUE(ctx, res);

    free_context(env);
}

TEST_CASE("JS_Call invokes C functions through stdlib", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue math = get_property(ctx, global, "Math");
    JSValue abs_func = get_property(ctx, math, "abs");
    REQUIRE(JS_IsFunction(ctx, abs_func));

    JSValue res = call_js(ctx, abs_func, math, {JS_NewInt32(ctx, -4)});
    REQUIRE(!JS_IsException(res));
    double out = 0.0;
    REQUIRE(JS_ToNumber(ctx, &out, res) == 0);
    CHECK(out == 4.0);

    free_context(env);
}

TEST_CASE("JS_Call rejects non-callable values", "[js_call]")
{
    TestContext env = make_context();
    JSContext *ctx = env.ctx;

    JSValue res = call_js(ctx, JS_NewInt32(ctx, 1), JS_UNDEFINED, {});
    REQUIRE(JS_IsException(res));

    free_context(env);
}
