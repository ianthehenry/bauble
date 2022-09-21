#include <emscripten.h>
#include <emscripten/bind.h>
#include <string>
#include "janet.h"

using std::string;

static JanetFunction *janetfn_bauble_evaluate = NULL;
static JanetFunction *janetfn_compile_shape = NULL;

JANET_FN(janet_function_arity, "(function/arity)", "") {
  janet_fixarity(argc, 1);
  const JanetFunction *function = janet_getfunction(argv, 0);
  return janet_wrap_number(function->def->arity);
}

JANET_FN(janet_function_min_arity, "(function/min-arity)", "") {
  janet_fixarity(argc, 1);
  const JanetFunction *function = janet_getfunction(argv, 0);
  return janet_wrap_number(function->def->min_arity);
}

JANET_FN(janet_function_max_arity, "(function/max-arity)", "") {
  janet_fixarity(argc, 1);
  const JanetFunction *function = janet_getfunction(argv, 0);
  return janet_wrap_number(function->def->max_arity);
}

JanetFunction *extract_function(JanetTable *env, const char *name) {
  JanetTable *declaration = janet_unwrap_table(janet_table_get(env, janet_csymbolv(name)));
  JanetFunction *function = janet_unwrap_function(janet_table_get(declaration, janet_ckeywordv("value")));
  if (!function) {
    fprintf(stderr, "failed to extract function %s\n", name);
  }
  return function;
}

Janet *call_fn(JanetFunction *fn, int argc, const Janet *argv) {
  Janet *result = (Janet *)malloc(sizeof(Janet));
  if (!result) {
    return NULL;
  }
  JanetFiber *fiber = NULL;
  if (janet_pcall(fn, argc, argv, result, &fiber) == JANET_SIGNAL_OK) {
    return result;
  } else {
    janet_stacktrace(fiber, *result);
    return NULL;
  }
}

struct CompilationResult {
  bool is_error;
  string shader_source;
  bool is_animated;
  string error;
};

CompilationResult compilation_error(string message) {
  return (CompilationResult) {
    .is_error = true,
    .shader_source = "",
    .is_animated = false,
    .error = message,
  };
}

CompilationResult evaluate_script(string source) {
  if (janetfn_compile_shape == NULL) {
    fprintf(stderr, "unable to initialize compilation function\n");
    return compilation_error("function uninitialized");
  }

  long long start_time = emscripten_get_now();
  Janet *evaluation_result = call_fn(janetfn_bauble_evaluate, 1, (Janet[1]){ janet_cstringv(source.c_str()) });
  if (!evaluation_result) {
    return compilation_error("evaluation error");
  }

  long long done_evaluating = emscripten_get_now();

  const Janet *tuple = janet_unwrap_tuple(*evaluation_result);
  const Janet compile_shape_args[2] = { tuple[0], tuple[1] };
  Janet *response_ptr = call_fn(janetfn_compile_shape, 2, compile_shape_args);
  free(evaluation_result);

  long long done_compiling_glsl = emscripten_get_now();
  bool is_animated;
  const uint8_t *shader_source;
  if (response_ptr) {
    Janet response = *response_ptr;
    if (janet_checktype(response, JANET_TUPLE)) {
      const Janet *tuple = janet_unwrap_tuple(response);
      is_animated = janet_unwrap_boolean(tuple[0]);
      shader_source = janet_unwrap_string(tuple[1]);
    } else if (janet_checktype(response, JANET_KEYWORD)) {
      free(response_ptr);
      return compilation_error("invalid value");
    } else {
      janet_eprintf("unexpected compilation result %p\n", response);
      free(response_ptr);
      return compilation_error("internal error");
    }
  } else {
    return compilation_error("compilation error");
  }

  printf("eval: %lldms compile: %lldms\n", (done_evaluating - start_time), (done_compiling_glsl - done_evaluating));

  return (CompilationResult) {
   .is_error = false,
   .shader_source = string((const char *)shader_source),
   .is_animated = is_animated,
   .error = ""
  };
}

EMSCRIPTEN_KEEPALIVE
int main() {
  janet_init();
  JanetTable *env = janet_core_env(NULL);

  const JanetRegExt regs[] = {
    JANET_REG("function/arity", janet_function_arity),
    JANET_REG("function/min-arity", janet_function_min_arity),
    JANET_REG("function/max-arity", janet_function_max_arity),
    JANET_REG_END
  };
  janet_cfuns_ext(env, NULL, regs);

  JanetFunction *dofile = extract_function(env, "dofile");
  Janet *result;
  bool error = false;

  result = call_fn(dofile, 1, (Janet[1]){ janet_cstringv("bauble-evaluator.janet") });
  if (result) {
    janetfn_bauble_evaluate = extract_function(janet_unwrap_table(*result), "evaluate");
    janet_gcroot(janet_wrap_function(janetfn_bauble_evaluate));
    free(result);
  } else {
    janet_eprintf("unable to extract compilation function\n");
    error = true;
  }

  result = call_fn(dofile, 1, (Janet[1]){ janet_cstringv("shade.janet") });
  if (result) {
    janetfn_compile_shape = extract_function(janet_unwrap_table(*result), "compile-shape");
    janet_gcroot(janet_wrap_function(janetfn_compile_shape));
    free(result);
  } else {
    janet_eprintf("unable to extract compilation function\n");
    error = true;
  }

  if (error) {
    janet_deinit();
  }
}


EMSCRIPTEN_BINDINGS(module) {
  using namespace emscripten;
  value_object<CompilationResult>("CompilationResult")
    .field("isError", &CompilationResult::is_error)
    .field("shaderSource", &CompilationResult::shader_source)
    .field("isAnimated", &CompilationResult::is_animated)
    .field("error", &CompilationResult::error)
    ;

  function("evaluate_script", &evaluate_script, allow_raw_pointers());
};
