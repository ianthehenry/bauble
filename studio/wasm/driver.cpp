#include <emscripten.h>
#include <emscripten/bind.h>
#include <string>
#include <stdio.h>
#include "janet.h"
#include "autocomplete.h"
#include "util.h"

using std::string;

static JanetFunction *janetfn_bauble_evaluate = NULL;
static JanetFunction *janetfn_compile_to_glsl = NULL;
static JanetFunction *janetfn_get_definitions = NULL;

struct CompilationResult {
  bool is_error;
  string shader_source;
  int dimension;
  bool is_animated;
  bool has_custom_camera;
  string error;
  double eval_time_ms;
  double compile_time_ms;
};

CompilationResult compilation_error(string message) {
  return (CompilationResult) {
    .is_error = true,
    .shader_source = "",
    .dimension = -1,
    .is_animated = false,
    .has_custom_camera = false,
    .error = message,
    .eval_time_ms = 0.0,
    .compile_time_ms = 0.0,
  };
}

CompilationResult evaluate_script(string source, int render_type) {
  if (janetfn_compile_to_glsl == NULL) {
    fprintf(stderr, "unable to initialize compilation function\n");
    return compilation_error("function uninitialized");
  }

  double start_time = emscripten_get_now();
  Janet compiled_env;
  const Janet args[1] = { janet_cstringv(source.c_str()) };
  if (!call_fn(janetfn_bauble_evaluate, 1, args, &compiled_env)) {
    return compilation_error("evaluation error");
  }

  double done_evaluating = emscripten_get_now();

  const Janet compile_to_glsl_args[3] = {
    janet_wrap_integer(render_type),
    compiled_env,
    janet_cstringv("300 es")
  };
  Janet compilation_result;
  bool compilation_success = call_fn(janetfn_compile_to_glsl, 3, compile_to_glsl_args, &compilation_result);

  double done_compiling_glsl = emscripten_get_now();
  const uint8_t *shader_source;
  int dimension;
  bool is_animated;
  bool has_custom_camera;
  if (compilation_success) {
    if (janet_checktype(compilation_result, JANET_TUPLE)) {
      const Janet *tuple = janet_unwrap_tuple(compilation_result);
      shader_source = janet_unwrap_string(tuple[0]);
      dimension = janet_unwrap_integer(tuple[1]);
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
   .dimension = dimension,
   .is_animated = is_animated,
   .has_custom_camera = has_custom_camera,
   .error = "",
   .eval_time_ms = (done_evaluating - start_time),
   .compile_time_ms = (done_compiling_glsl - done_evaluating)
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
}

std::vector<Definition> get_definitions_aux() {
  return get_definitions(janetfn_get_definitions);
}

EMSCRIPTEN_BINDINGS(module) {
  using namespace emscripten;
  value_object<CompilationResult>("CompilationResult")
    .field("isError", &CompilationResult::is_error)
    .field("shaderSource", &CompilationResult::shader_source)
    .field("dimension", &CompilationResult::dimension)
    .field("isAnimated", &CompilationResult::is_animated)
    .field("hasCustomCamera", &CompilationResult::has_custom_camera)
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
