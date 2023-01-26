#include <emscripten.h>
#include <emscripten/bind.h>
#include <string>
#include <stdio.h>
#include "janet.h"

using std::string;

static JanetFunction *janetfn_bauble_evaluate = NULL;
static JanetFunction *janetfn_compile_shape = NULL;

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

unsigned char *read_file(const char *filename, size_t *length) {
  // choosing deliberately to resize...
  size_t capacity = 2 << 16;
  unsigned char *src = (unsigned char *)malloc(capacity * sizeof(unsigned char));
  assert(src);
  size_t total_bytes_read = 0;
  FILE *file = fopen(filename, "r");
  assert(file);
  while (true) {
    size_t remaining_capacity = capacity - total_bytes_read;
    if (remaining_capacity == 0) {
      capacity <<= 1;
      src = (unsigned char *)realloc(src, capacity * sizeof(unsigned char));
      assert(src);
      remaining_capacity = capacity - total_bytes_read;
    }

    size_t bytes_read = fread(&src[total_bytes_read], sizeof(unsigned char), remaining_capacity, file);
    total_bytes_read += bytes_read;
    if (bytes_read == 0) {
      break;
    }
  }
  fclose(file);
  *length = total_bytes_read;
  return src;
}

EMSCRIPTEN_KEEPALIVE
int main() {
  janet_init();
  JanetTable *core_env = janet_core_env(NULL);
  // TODO: shouldn't this be load-image-dict?
  JanetTable *lookup = janet_env_lookup(core_env);

  size_t bauble_image_length;
  unsigned char *bauble_image = read_file("bauble.jimage", &bauble_image_length);

  Janet bauble = janet_unmarshal(bauble_image, bauble_image_length, 0, lookup, NULL);
  if (!janet_checktype(bauble, JANET_TABLE)) {
    janet_panicf("invalid image %q", bauble);
  }

  janetfn_bauble_evaluate = extract_function(janet_unwrap_table(bauble), "bauble-evaluator/evaluate");
  janet_gcroot(janet_wrap_function(janetfn_bauble_evaluate));
  janetfn_compile_shape = extract_function(janet_unwrap_table(bauble), "shade/compile-shape");
  janet_gcroot(janet_wrap_function(janetfn_compile_shape));
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
