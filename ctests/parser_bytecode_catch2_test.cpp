#include <catch2/catch_test_macros.hpp>

#include <cstddef>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>

extern "C" {
#include "mquickjs.h"
#include "mquickjs_priv.h"
extern const JSSTDLibraryDef js_stdlib;
}

#ifndef JS_MB_PAD
#define JS_MB_PAD(n) (JSW * 8 - (n))
#endif

typedef struct {
    JS_MB_HEADER;
    JSWord size: JS_MB_PAD(JS_MTAG_BITS);
    uint8_t buf[];
} JSByteArray;

typedef struct {
    JS_MB_HEADER;
    JSWord size: JS_MB_PAD(JS_MTAG_BITS);
    JSValue arr[];
} JSValueArray;

typedef struct {
    JS_MB_HEADER;
    JSWord is_unique: 1;
    JSWord is_ascii: 1;
    JSWord is_numeric: 1;
    JSWord len: JS_MB_PAD(JS_MTAG_BITS + 3);
    uint8_t buf[];
} JSString;

typedef struct {
    JS_MB_HEADER;
    JSWord has_arguments : 1;
    JSWord has_local_func_name : 1;
    JSWord has_column : 1;
    JSWord arg_count : 16;
    JSWord dummy: JS_MB_PAD(JS_MTAG_BITS + 3 + 16);

    JSValue func_name;
    JSValue byte_code;
    JSValue cpool;
    JSValue vars;
    JSValue ext_vars;
    uint16_t stack_size;
    uint16_t ext_vars_len;
    JSValue filename;
    JSValue pc2line;
    uint32_t source_pos;
} JSFunctionBytecode;

enum OpCodeEnum {
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f) OP_##id,
#include "mquickjs_opcode.h"
#undef DEF
#undef FMT
};

static const uint8_t OPCODE_SIZE[] = {
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f) size,
#include "mquickjs_opcode.h"
#undef DEF
#undef FMT
};

static const char *OPCODE_NAMES[] = {
#define FMT(f)
#define DEF(id, size, n_pop, n_push, f) #id,
#include "mquickjs_opcode.h"
#undef DEF
#undef FMT
};

static std::vector<uint8_t> decode_ops(const uint8_t *buf, size_t len)
{
    std::vector<uint8_t> ops;
    size_t pos = 0;
    while (pos < len) {
        uint8_t op = buf[pos];
        REQUIRE(op < sizeof(OPCODE_SIZE));
        uint8_t size = OPCODE_SIZE[op];
        REQUIRE(size > 0);
        REQUIRE(pos + size <= len);
        ops.push_back(op);
        pos += size;
    }
    REQUIRE(pos == len);
    return ops;
}

static std::string ops_to_string(const std::vector<uint8_t> &ops)
{
    std::string out;
    for (size_t i = 0; i < ops.size(); i++) {
        uint8_t op = ops[i];
        const char *name = op < (sizeof(OPCODE_NAMES) / sizeof(OPCODE_NAMES[0]))
            ? OPCODE_NAMES[op]
            : "unknown";
        if (i > 0) {
            out.push_back(' ');
        }
        out.append(name);
    }
    return out;
}

struct CompiledProgram {
    void *mem = nullptr;
    JSContext *ctx = nullptr;
    JSBytecodeHeader header{};
    const uint8_t *data_buf = nullptr;
    uint32_t data_len = 0;
};

static CompiledProgram compile_source(const char *source)
{
    const size_t mem_size = 4 * 1024 * 1024;
    void *mem = std::malloc(mem_size);
    REQUIRE(mem != nullptr);

    JSContext *ctx = JS_NewContext2(mem, mem_size, &js_stdlib, 1);
    REQUIRE(ctx != nullptr);

    JSValue val = JS_Parse(ctx, source, std::strlen(source), "<test>", 0);
    REQUIRE(!JS_IsException(val));

    CompiledProgram prog;
    prog.mem = mem;
    prog.ctx = ctx;
    JS_PrepareBytecode(ctx, &prog.header, &prog.data_buf, &prog.data_len, val);
    REQUIRE(prog.header.magic == JS_BYTECODE_MAGIC);
    REQUIRE(prog.data_len > 0);
    return prog;
}

static void free_program(CompiledProgram &prog)
{
    if (prog.ctx) {
        JS_FreeContext(prog.ctx);
        prog.ctx = nullptr;
    }
    if (prog.mem) {
        std::free(prog.mem);
        prog.mem = nullptr;
    }
}

static const JSString *value_as_string(JSValue val)
{
    if (!JS_IsPtr(val)) {
        return nullptr;
    }
    auto *str = static_cast<JSString *>(JS_VALUE_TO_PTR(val));
    if (str->mtag != JS_MTAG_STRING) {
        return nullptr;
    }
    return str;
}

static std::string string_value(JSValue val)
{
    const JSString *str = value_as_string(val);
    if (!str) {
        return {};
    }
    return std::string(reinterpret_cast<const char *>(str->buf), str->len);
}

static const JSFunctionBytecode *find_function_by_name(const JSFunctionBytecode *root,
                                                       const char *name)
{
    if (root->cpool == JS_NULL || !JS_IsPtr(root->cpool)) {
        return nullptr;
    }
    auto *cpool = static_cast<JSValueArray *>(JS_VALUE_TO_PTR(root->cpool));
    if (cpool->mtag != JS_MTAG_VALUE_ARRAY) {
        return nullptr;
    }

    for (size_t i = 0; i < static_cast<size_t>(cpool->size); i++) {
        JSValue entry = cpool->arr[i];
        if (!JS_IsPtr(entry)) {
            continue;
        }
        auto *func = static_cast<JSFunctionBytecode *>(JS_VALUE_TO_PTR(entry));
        if (func->mtag != JS_MTAG_FUNCTION_BYTECODE) {
            continue;
        }
        if (string_value(func->func_name) == name) {
            return func;
        }
    }
    return nullptr;
}

static const JSByteArray *function_bytecode(const JSFunctionBytecode *func)
{
    REQUIRE(JS_IsPtr(func->byte_code));
    auto *byte_array = static_cast<JSByteArray *>(JS_VALUE_TO_PTR(func->byte_code));
    REQUIRE(byte_array->mtag == JS_MTAG_BYTE_ARRAY);
    REQUIRE(byte_array->size > 0);
    return byte_array;
}

static std::string bytes_to_hex(const uint8_t *buf, size_t len)
{
    static const char kHexDigits[] = "0123456789abcdef";
    std::string out;
    if (len == 0) {
        return out;
    }
    out.reserve(len * 3 - 1);
    for (size_t i = 0; i < len; i++) {
        if (i > 0) {
            out.push_back(' ');
        }
        uint8_t byte = buf[i];
        out.push_back(kHexDigits[byte >> 4]);
        out.push_back(kHexDigits[byte & 0x0f]);
    }
    return out;
}


TEST_CASE("parser emits bytecode from JS source", "[parser][bytecode]")
{
    const char *source = "var x = 1; x = x + 2;";
    CompiledProgram prog = compile_source(source);

    std::vector<uint8_t> buffer(sizeof(prog.header) + prog.data_len);
    std::memcpy(buffer.data(), &prog.header, sizeof(prog.header));
    std::memcpy(buffer.data() + sizeof(prog.header), prog.data_buf, prog.data_len);

    CHECK(JS_IsBytecode(buffer.data(), buffer.size()));

    REQUIRE(JS_IsPtr(prog.header.main_func));
    auto *func = static_cast<JSFunctionBytecode *>(JS_VALUE_TO_PTR(prog.header.main_func));
    REQUIRE(func->mtag == JS_MTAG_FUNCTION_BYTECODE);
    const JSByteArray *byte_array = function_bytecode(func);

    auto ops = decode_ops(byte_array->buf, static_cast<size_t>(byte_array->size));
    const std::vector<uint8_t> expected = {
        static_cast<uint8_t>(OP_push_1),
        static_cast<uint8_t>(OP_put_var_ref_nocheck),
        static_cast<uint8_t>(OP_get_var_ref),
        static_cast<uint8_t>(OP_push_2),
        static_cast<uint8_t>(OP_add),
        static_cast<uint8_t>(OP_put_var_ref),
        static_cast<uint8_t>(OP_return_undef),
    };
    INFO("ops: " << ops_to_string(ops));
    REQUIRE(ops == expected);

    free_program(prog);
}

TEST_CASE("parser emits dup2 for array element post-inc", "[parser][bytecode]")
{
    const char *source = R"JS(
function test_inc_array()
{
    var a;
    a = [true];
    a[0]++;
}
)JS";

    CompiledProgram prog = compile_source(source);
    REQUIRE(JS_IsPtr(prog.header.main_func));
    auto *root = static_cast<JSFunctionBytecode *>(JS_VALUE_TO_PTR(prog.header.main_func));

    const JSFunctionBytecode *func = find_function_by_name(root, "test_inc_array");
    REQUIRE(func != nullptr);
    const JSByteArray *byte_array = function_bytecode(func);
    auto ops = decode_ops(byte_array->buf, static_cast<size_t>(byte_array->size));
    INFO("ops: " << ops_to_string(ops));

    bool has_dup2_get_array_el = false;
    for (size_t i = 1; i < ops.size(); i++) {
        if (ops[i - 1] == static_cast<uint8_t>(OP_dup2)
            && ops[i] == static_cast<uint8_t>(OP_get_array_el)) {
            has_dup2_get_array_el = true;
            break;
        }
    }
    REQUIRE(has_dup2_get_array_el);

    free_program(prog);
}

TEST_CASE("parser emits bytecode for loop functions", "[parser][bytecode]")
{
    const char *source = R"JS(
function test_while()
{
    var i, c;
    i = 0;
    c = 0;
    while (i < 3) {
        c++;
        i++;
    }
    assert(c === 3);
}

function test_while_break()
{
    var i, c;
    i = 0;
    c = 0;
    while (i < 3) {
        c++;
        if (i == 1)
            break;
        i++;
    }
    assert(c === 2 && i === 1);
}

function test_do_while()
{
    var i, c;
    i = 0;
    c = 0;
    do {
        c++;
        i++;
    } while (i < 3);
    assert(c === 3 && i === 3);
}
)JS";

    CompiledProgram prog = compile_source(source);
    REQUIRE(JS_IsPtr(prog.header.main_func));
    auto *root = static_cast<JSFunctionBytecode *>(JS_VALUE_TO_PTR(prog.header.main_func));

    const JSFunctionBytecode *func = find_function_by_name(root, "test_while");
    REQUIRE(func != nullptr);
    const JSByteArray *byte_array = function_bytecode(func);
    auto ops = decode_ops(byte_array->buf, static_cast<size_t>(byte_array->size));
    INFO("test_while ops: " << ops_to_string(ops));
    const std::vector<uint8_t> expected_while = {
        static_cast<uint8_t>(OP_push_0),
        static_cast<uint8_t>(OP_put_loc0),
        static_cast<uint8_t>(OP_push_0),
        static_cast<uint8_t>(OP_put_loc1),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_push_3),
        static_cast<uint8_t>(OP_lt),
        static_cast<uint8_t>(OP_if_false),
        static_cast<uint8_t>(OP_get_loc1),
        static_cast<uint8_t>(OP_inc),
        static_cast<uint8_t>(OP_put_loc1),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_inc),
        static_cast<uint8_t>(OP_put_loc0),
        static_cast<uint8_t>(OP_goto),
        static_cast<uint8_t>(OP_get_var_ref),
        static_cast<uint8_t>(OP_get_loc1),
        static_cast<uint8_t>(OP_push_3),
        static_cast<uint8_t>(OP_strict_eq),
        static_cast<uint8_t>(OP_call),
        static_cast<uint8_t>(OP_drop),
        static_cast<uint8_t>(OP_return_undef),
    };
    REQUIRE(ops == expected_while);

    func = find_function_by_name(root, "test_while_break");
    REQUIRE(func != nullptr);
    byte_array = function_bytecode(func);
    ops = decode_ops(byte_array->buf, static_cast<size_t>(byte_array->size));
    INFO("test_while_break ops: " << ops_to_string(ops));
    const std::vector<uint8_t> expected_while_break = {
        static_cast<uint8_t>(OP_push_0),
        static_cast<uint8_t>(OP_put_loc0),
        static_cast<uint8_t>(OP_push_0),
        static_cast<uint8_t>(OP_put_loc1),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_push_3),
        static_cast<uint8_t>(OP_lt),
        static_cast<uint8_t>(OP_if_false),
        static_cast<uint8_t>(OP_get_loc1),
        static_cast<uint8_t>(OP_inc),
        static_cast<uint8_t>(OP_put_loc1),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_push_1),
        static_cast<uint8_t>(OP_eq),
        static_cast<uint8_t>(OP_if_false),
        static_cast<uint8_t>(OP_goto),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_inc),
        static_cast<uint8_t>(OP_put_loc0),
        static_cast<uint8_t>(OP_goto),
        static_cast<uint8_t>(OP_get_var_ref),
        static_cast<uint8_t>(OP_get_loc1),
        static_cast<uint8_t>(OP_push_2),
        static_cast<uint8_t>(OP_strict_eq),
        static_cast<uint8_t>(OP_dup),
        static_cast<uint8_t>(OP_if_false),
        static_cast<uint8_t>(OP_drop),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_push_1),
        static_cast<uint8_t>(OP_strict_eq),
        static_cast<uint8_t>(OP_call),
        static_cast<uint8_t>(OP_drop),
        static_cast<uint8_t>(OP_return_undef),
    };
    REQUIRE(ops == expected_while_break);

    func = find_function_by_name(root, "test_do_while");
    REQUIRE(func != nullptr);
    byte_array = function_bytecode(func);
    ops = decode_ops(byte_array->buf, static_cast<size_t>(byte_array->size));
    INFO("test_do_while ops: " << ops_to_string(ops));
    const std::vector<uint8_t> expected_do_while = {
        static_cast<uint8_t>(OP_push_0),
        static_cast<uint8_t>(OP_put_loc0),
        static_cast<uint8_t>(OP_push_0),
        static_cast<uint8_t>(OP_put_loc1),
        static_cast<uint8_t>(OP_get_loc1),
        static_cast<uint8_t>(OP_inc),
        static_cast<uint8_t>(OP_put_loc1),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_inc),
        static_cast<uint8_t>(OP_put_loc0),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_push_3),
        static_cast<uint8_t>(OP_lt),
        static_cast<uint8_t>(OP_if_true),
        static_cast<uint8_t>(OP_get_var_ref),
        static_cast<uint8_t>(OP_get_loc1),
        static_cast<uint8_t>(OP_push_3),
        static_cast<uint8_t>(OP_strict_eq),
        static_cast<uint8_t>(OP_dup),
        static_cast<uint8_t>(OP_if_false),
        static_cast<uint8_t>(OP_drop),
        static_cast<uint8_t>(OP_get_loc0),
        static_cast<uint8_t>(OP_push_3),
        static_cast<uint8_t>(OP_strict_eq),
        static_cast<uint8_t>(OP_call),
        static_cast<uint8_t>(OP_drop),
        static_cast<uint8_t>(OP_return_undef),
    };
    REQUIRE(ops == expected_do_while);

    free_program(prog);
}

TEST_CASE("parser emits regexp bytecode for tag pattern", "[parser][regexp][bytecode]")
{
    const char *pattern = "<(\\/)?([^<>]+)>";
    constexpr size_t kMemSize = 4 * 1024 * 1024;
    void *mem = std::malloc(kMemSize);
    REQUIRE(mem != nullptr);

    JSContext *ctx = JS_NewContext2(mem, kMemSize, &js_stdlib, 1);
    REQUIRE(ctx != nullptr);

    const int re_flags = 0;
    JSValue val = JS_Parse(ctx,
                           pattern,
                           std::strlen(pattern),
                           "<regexp>",
                           JS_EVAL_REGEXP | (re_flags << JS_EVAL_REGEXP_FLAGS_SHIFT));
    REQUIRE(!JS_IsException(val));
    REQUIRE(JS_IsPtr(val));

    auto *byte_array = static_cast<JSByteArray *>(JS_VALUE_TO_PTR(val));
    REQUIRE(byte_array->mtag == JS_MTAG_BYTE_ARRAY);
    REQUIRE(byte_array->size > 0);

    std::string hex = bytes_to_hex(byte_array->buf, static_cast<size_t>(byte_array->size));
    std::fprintf(stdout, "regexp bytecode (%s): %s\n", pattern, hex.c_str());

    JS_FreeContext(ctx);
    std::free(mem);
}
