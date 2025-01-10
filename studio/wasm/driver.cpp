#include <emscripten.h>
#include <emscripten/bind.h>
#include <string>
#include <array>
#include <stdio.h>
#include "janet.h"
#include "autocomplete.h"
#include "util.h"

using std::string;

static JanetFunction *janetfn_bauble_evaluate = NULL;
static JanetFunction *janetfn_compile_to_glsl = NULL;
static JanetFunction *janetfn_get_definitions = NULL;
static JanetFunction *janetfn_get_uniforms = NULL;

union Value {
  double float_;
  double vec2[2];
  double vec3[3];
  double vec4[4];
};

struct Uniform {
  string name;
  string type;
  Value value;
};

struct CompilationResult {
  bool is_error;
  string shader_source;
  int dimensions;
  bool is_animated;
  bool has_custom_camera;
  std::vector<Uniform> uniforms;
  string error;
  double eval_time_ms;
  double compile_time_ms;
};

CompilationResult compilation_error(string message) {
  return (CompilationResult) {
    .is_error = true,
    .shader_source = "",
    .dimensions = -1,
    .is_animated = false,
    .has_custom_camera = false,
    .uniforms = std::vector<Uniform>(),
    .error = message,
    .eval_time_ms = 0.0,
    .compile_time_ms = 0.0,
  };
}

CompilationResult evaluate_script(string source, int render_type, bool crosshairs, bool dynamic_camera) {
  if (janetfn_compile_to_glsl == NULL || janetfn_get_uniforms == NULL) {
    fprintf(stderr, "unable to initialize compilation function\n");
    return compilation_error("function uninitialized");
  }

  double start_time = emscripten_get_now();
  Janet compiled_env;
  const Janet evaluate_args[1] = { janet_cstringv(source.c_str()) };
  if (!call_fn(janetfn_bauble_evaluate, 1, evaluate_args, &compiled_env)) {
    return compilation_error("evaluation error");
  }
  janet_gcroot(compiled_env);

  Janet uniforms_value;
  const Janet get_uniforms_args[1] = { compiled_env };
  if (!call_fn(janetfn_get_uniforms, 1, get_uniforms_args, &uniforms_value)) {
    janet_gcunroot(compiled_env);
    return compilation_error("evaluation error");
  }

  JanetArray *uniforms_array = janet_unwrap_array(uniforms_value);
  int32_t uniform_count = uniforms_array->count;
  auto uniforms_vec = std::vector<Uniform>();
  for (int32_t i = 0; i < uniform_count; i++) {
    const Janet *uniform = janet_unwrap_tuple(uniforms_array->data[i]);
    const JanetString name = janet_unwrap_string(uniform[0]);
    const JanetString type = janet_unwrap_string(uniform[1]);
    const Janet janet_value = uniform[2];
    Value value;

    if (janet_string_equal(type, janet_cstring("float"))) {
      value = (Value){.float_ = janet_unwrap_number(janet_value)};
    } else if (janet_string_equal(type, janet_cstring("vec2"))) {
      const Janet *vec2 = janet_unwrap_tuple(janet_value);
      value = (Value){.vec2 = {janet_unwrap_number(vec2[0]), janet_unwrap_number(vec2[1])}};
    } else if (janet_string_equal(type, janet_cstring("vec3"))) {
      const Janet *vec3 = janet_unwrap_tuple(janet_value);
      value = (Value){.vec3 = {janet_unwrap_number(vec3[0]), janet_unwrap_number(vec3[1]), janet_unwrap_number(vec3[2])}};
    } else if (janet_string_equal(type, janet_cstring("vec4"))) {
      const Janet *vec4 = janet_unwrap_tuple(janet_value);
      value = (Value){.vec4 = {janet_unwrap_number(vec4[0]), janet_unwrap_number(vec4[1]), janet_unwrap_number(vec4[2]), janet_unwrap_number(vec4[3])}};
    } else {
      janet_gcunroot(compiled_env);
      return compilation_error("BUG: unknown type");
    }

    uniforms_vec.push_back((Uniform) {
      .name = string((const char *)name),
      .type = string((const char *)type),
      .value = value,
    });
  }

  double done_evaluating = emscripten_get_now();

  const size_t arg_count = 5;
  const Janet compile_to_glsl_args[arg_count] = {
    janet_wrap_integer(render_type),
    janet_wrap_boolean(crosshairs),
    janet_wrap_boolean(dynamic_camera),
    compiled_env,
    janet_cstringv("300 es")
  };
  Janet compilation_result;
  bool compilation_success = call_fn(janetfn_compile_to_glsl, arg_count, compile_to_glsl_args, &compilation_result);
  janet_gcunroot(compiled_env);

  double done_compiling_glsl = emscripten_get_now();
  const uint8_t *shader_source;
  int dimensions;
  bool is_animated;
  bool has_custom_camera;
  if (compilation_success) {
    if (janet_checktype(compilation_result, JANET_TUPLE)) {
      const Janet *tuple = janet_unwrap_tuple(compilation_result);
      shader_source = janet_unwrap_string(tuple[0]);
      dimensions = janet_unwrap_integer(tuple[1]);
      is_animated = janet_unwrap_boolean(tuple[2]);
      has_custom_camera = janet_unwrap_boolean(tuple[3]);
    } else if (janet_checktype(compilation_result, JANET_KEYWORD)) {
      return compilation_error("invalid value");
    } else {
      janet_eprintf("unexpected compilation result %p\n", compilation_result);
      return compilation_error("internal error");
    }
  } else {
    return compilation_error("compilation error");
  }

  return (CompilationResult) {
   .is_error = false,
   .shader_source = string((const char *)shader_source),
   .dimensions = dimensions,
   .is_animated = is_animated,
   .has_custom_camera = has_custom_camera,
   .uniforms = uniforms_vec,
   .error = "",
   .eval_time_ms = (done_evaluating - start_time),
   .compile_time_ms = (done_compiling_glsl - done_evaluating),
  };
}

EMSCRIPTEN_KEEPALIVE
int main() {
  janet_init();
  JanetTable *core_env = janet_core_env(NULL);
  // TODO: shouldn't this be load-image-dict?
  JanetTable *lookup = janet_env_lookup(core_env);

  size_t bauble_image_length;
  unsigned char *bauble_image = read_file("bauble.jimage", &bauble_image_length);

  Janet env = janet_unmarshal(bauble_image, bauble_image_length, 0, lookup, NULL);
  if (!janet_checktype(env, JANET_TABLE)) {
    janet_panicf("invalid image %q", env);
  }
  JanetTable *bauble = janet_unwrap_table(env);

  janetfn_bauble_evaluate = env_lookup_function(bauble, "evaluator/evaluate");
  janet_gcroot(janet_wrap_function(janetfn_bauble_evaluate));

  janetfn_get_definitions = env_lookup_function(bauble, "completer/get-definitions");
  janet_gcroot(janet_wrap_function(janetfn_get_definitions));

  janetfn_compile_to_glsl = env_lookup_function(bauble, "compile-to-glsl");
  janet_gcroot(janet_wrap_function(janetfn_compile_to_glsl));

  janetfn_get_uniforms = env_lookup_function(bauble, "get-uniforms");
  janet_gcroot(janet_wrap_function(janetfn_get_uniforms));
}

std::vector<Definition> get_definitions_aux() {
  return get_definitions(janetfn_get_definitions);
}

EMSCRIPTEN_BINDINGS(module) {
  using namespace emscripten;

  value_array<std::array<double, 2>>("vec2")
    .element(emscripten::index<0>())
    .element(emscripten::index<1>())
    ;
  value_array<std::array<double, 3>>("vec3")
    .element(emscripten::index<0>())
    .element(emscripten::index<1>())
    .element(emscripten::index<2>())
    ;
  value_array<std::array<double, 4>>("vec4")
    .element(emscripten::index<0>())
    .element(emscripten::index<1>())
    .element(emscripten::index<2>())
    .element(emscripten::index<3>())
    ;

  value_object<Value>("Value")
    .field("float", &Value::float_)
    .field("vec2", &Value::vec2)
    .field("vec3", &Value::vec3)
    .field("vec4", &Value::vec4)
    ;
  value_object<Uniform>("Uniform")
    .field("name", &Uniform::name)
    .field("type", &Uniform::type)
    .field("value", &Uniform::value)
    ;

  register_vector<Uniform>("UniformVector");
  value_object<CompilationResult>("CompilationResult")
    .field("isError", &CompilationResult::is_error)
    .field("shaderSource", &CompilationResult::shader_source)
    .field("dimensions", &CompilationResult::dimensions)
    .field("isAnimated", &CompilationResult::is_animated)
    .field("hasCustomCamera", &CompilationResult::has_custom_camera)
    .field("uniforms", &CompilationResult::uniforms)
    .field("error", &CompilationResult::error)
    .field("evalTimeMs", &CompilationResult::eval_time_ms)
    .field("compileTimeMs", &CompilationResult::compile_time_ms)
    ;
  value_object<Definition>("Definition")
    .field("name", &Definition::name)
    .field("args", &Definition::args)
    .field("doc", &Definition::doc)
    .field("type", &Definition::type)
    ;

  register_vector<Definition>("DefinitionVector");

  function("evaluateScript", &evaluate_script, allow_raw_pointers());
  function("getDefinitions", &get_definitions_aux, allow_raw_pointers());
};
