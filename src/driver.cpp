#include <emscripten.h>
#include "janet.h"

#define GL_DEBUG

#include <GLES2/gl2.h>
#include <EGL/egl.h>
#include <emscripten/html5.h>
#include <math.h>

typedef struct {
  GLfloat v[3];
} vec3;

typedef struct {
  GLfloat v[9];
} mat3;

vec3 mat3_times_vec3(mat3 x, vec3 y) {
  return (vec3) {
    .v = {
      x.v[0*3+0] * y.v[0] + x.v[0*3+1] * y.v[1] + x.v[0*3+2] * y.v[2],
      x.v[1*3+0] * y.v[0] + x.v[1*3+1] * y.v[1] + x.v[1*3+2] * y.v[2],
      x.v[2*3+0] * y.v[0] + x.v[2*3+1] * y.v[1] + x.v[2*3+2] * y.v[2],
    }
  };
}

mat3 rotate_xy(float x, float y) {
  float sx = sin(x);
  float sy = sin(y);
  float cx = cos(x);
  float cy = cos(y);

  return (mat3) {
    .v = {
      cy , sy * sx, sy * cx,
      0.0,      cx,     -sx,
      -sy, cy * sx, cy * cx
    }
  };
}

typedef struct {
  EMSCRIPTEN_WEBGL_CONTEXT_HANDLE handle;
  GLuint program;
  GLuint current_fragment_shader;
  vec3 camera_origin;
  mat3 camera_matrix;
  GLfloat time;
  GLint view_type;
} gl_context;

static JanetFunction *compile = NULL;
static gl_context *global_context = NULL;

GLuint compile_shader(GLenum type, const char *source) {
  GLuint shader = glCreateShader(type);

  if (shader == 0) {
    return 0;
  }

  const GLchar *sources[1];
  sources[0] = source;

  glShaderSource(shader, 1, sources, NULL);
  glCompileShader(shader);

  int info_length = 0;
  glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &info_length);

  if (info_length > 0) {
    char *info_msg = (char *) malloc(sizeof(char) * info_length);
    glGetShaderInfoLog(shader, info_length, NULL, info_msg);
    fprintf(stderr, "%s\n", info_msg);
    free(info_msg);
  }

  return shader;
}

void set_all_uniforms(gl_context *ctx) {
  emscripten_webgl_make_context_current(ctx->handle);
  glUseProgram(ctx->program);
  GLuint camera_matrix_uniform = glGetUniformLocation(ctx->program, "camera_matrix");
  GLuint camera_origin_uniform = glGetUniformLocation(ctx->program, "camera_origin");
  GLuint t_uniform = glGetUniformLocation(ctx->program, "t");
  GLuint view_type_uniform = glGetUniformLocation(ctx->program, "view_type");
  glUniform3fv(camera_origin_uniform, 1, ctx->camera_origin.v);
  glUniformMatrix3fv(camera_matrix_uniform, 1, 1, ctx->camera_matrix.v);
  glUniform1f(t_uniform, ctx->time);
  glUniform1i(view_type_uniform, ctx->view_type);
}

void draw_triangles(gl_context *context) {
  emscripten_webgl_make_context_current(context->handle);
  set_all_uniforms(context);

  const GLfloat width = 1024.0;
  const GLfloat height = 1024.0;

  const GLfloat left = -0.5 * width;
  const GLfloat right = 0.5 * width;
  const GLfloat top = 0.5 * height;
  const GLfloat bottom = -0.5 * height;

  GLfloat vertices[] = { left, top, 0.0,
                         right, top, 0.0,
                         right, bottom, 0.0,
                         left, bottom, 0.0 };
  GLushort indices[] = { 0, 1, 2, 0, 2, 3 };

  GLint positionLoc = glGetAttribLocation(context->program, "position");

  GLuint vertexBuffer;
  glGenBuffers(1, &vertexBuffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  GLuint indexBuffer;
  glGenBuffers(1, &indexBuffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);

  glViewport(0, 0, width, height);
  glClear(GL_COLOR_BUFFER_BIT);
  glVertexAttribPointer(positionLoc, 3, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(positionLoc);

  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, 0);
}

// caller must free result
char *slurp(const char *filename) {
  FILE *file = fopen(filename, "r");
  fseek(file, 0L, SEEK_END);
  long length = ftell(file);
  fseek(file, 0L, SEEK_SET);

  char *text = (char *)calloc(length + 1, sizeof(char));

  long bytes_read = 0;
  while (bytes_read < length) {
    bytes_read += fread(text, sizeof(char), length, file);
  }
  fclose(file);
  text[length] = 0;
  return text;
}

gl_context *new_gl_context(const char *selector) {
  EmscriptenWebGLContextAttributes attrs;
  emscripten_webgl_init_context_attributes(&attrs);
  attrs.antialias = 0;
  // attrs.alpha = 0;
  attrs.depth = 0;
  attrs.stencil = 0;
  attrs.majorVersion = 2;

  EMSCRIPTEN_WEBGL_CONTEXT_HANDLE handle = emscripten_webgl_create_context(selector, &attrs);

  if (handle <= 0) {
    fprintf(stderr, "failed to create context %d\n", handle);
  }

  emscripten_webgl_make_context_current(handle);

  GLuint program = glCreateProgram();

  const char *vertex_shader_source =
    "#version 300 es\n"
    "in vec4 position;\n"
    "void main() {\n"
    "  gl_Position = position;\n"
    "}\n";
  GLuint vertex_shader = compile_shader(GL_VERTEX_SHADER, vertex_shader_source);

  const char *fragment_shader_source =
    "#version 300 es\n"
    "precision highp float;"
    "in vec4 position;\n"
    "out vec4 frag_color;\n"
    "void main() {\n"
    "  frag_color = vec4(0.0);\n"
    "}\n";
  GLuint fragment_shader = compile_shader(GL_FRAGMENT_SHADER, fragment_shader_source);

  glAttachShader(program, vertex_shader);
  glAttachShader(program, fragment_shader);

  // TODO: currently no way to tear down the graphics context and free this memory
  gl_context *context = (gl_context *)calloc(1, sizeof(*context));
  context->handle = handle;
  context->program = program;
  context->current_fragment_shader = fragment_shader;

  return context;
}

void set_fragment_shader(gl_context *context, const uint8_t *shader_source) {
  glDetachShader(context->program, context->current_fragment_shader);
  glDeleteShader(context->current_fragment_shader);

  GLuint fragment_shader = compile_shader(GL_FRAGMENT_SHADER, (char *)shader_source);

  glAttachShader(context->program, fragment_shader);
  glLinkProgram(context->program);
  emscripten_webgl_make_context_current(context->handle);
  glUseProgram(context->program);

  context->current_fragment_shader = fragment_shader;
}

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
  Janet anything = janet_table_get(env, janet_csymbolv(name));
  JanetTable *declaration = janet_unwrap_table(janet_table_get(env, janet_csymbolv(name)));
  return janet_unwrap_function(janet_table_get(declaration, janet_ckeywordv("value")));
}

extern "C" {

void initialize_janet() {
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
  const Janet args[1] = { janet_cstringv("shade.janet") };
  Janet result;
  JanetFiber *fiber = NULL;
  JanetSignal compilation_result = janet_pcall(dofile, 1, args, &result, &fiber);
  if (compilation_result == 0) {
    compile = extract_function(janet_unwrap_table(result), "compile-shape");
    janet_gcroot(janet_wrap_function(compile));
  } else {
    janet_eprintf("error getting compilation function\n");
    janet_stacktrace(fiber, result);
    janet_deinit();
  }

  global_context = new_gl_context("#render-target");
}

EMSCRIPTEN_KEEPALIVE
void update_camera(float x, float y, float zoom) {
  mat3 camera_matrix = rotate_xy(x, y);
  vec3 camera_origin = { .v = { 0.0, 0.0, (GLfloat)(256.0 * zoom) } };
  camera_origin = mat3_times_vec3(camera_matrix, camera_origin);

  gl_context *ctx = global_context;
  ctx->camera_origin = camera_origin;
  ctx->camera_matrix = camera_matrix;
}

EMSCRIPTEN_KEEPALIVE
void update_time(float t) {
  gl_context *ctx = global_context;
  ctx->time = t;
}

EMSCRIPTEN_KEEPALIVE
void update_view_type(int view_type) {
  gl_context *ctx = global_context;
  ctx->view_type = view_type;
}

EMSCRIPTEN_KEEPALIVE
void rerender(float x, float y, float zoom) {
  draw_triangles(global_context);
}

EMSCRIPTEN_KEEPALIVE
void recompile_fragment_shader(const char *source) {
  set_fragment_shader(global_context, (const uint8_t *)source);
}

}

#include <emscripten/bind.h>
#include <string>

using std::string;

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
  long long start_time = emscripten_get_now();

  if (compile == NULL) {
    fprintf(stderr, "unable to initialize compilation function\n");
    return compilation_error("function uninitialized");
  }

  JanetTable *env = janet_core_env(NULL);

  Janet user_expression;
  JanetSignal status = (JanetSignal)janet_dostring(env, source.c_str(), "script", &user_expression);

  long long done_evaluating = emscripten_get_now();

  if (status != JANET_SIGNAL_OK) {
    return compilation_error("evaluation error");
  }

  const Janet args[1] = { user_expression };
  Janet response;
  JanetFiber *execution_fiber = NULL;
  status = janet_pcall(compile, 1, args, &response, &execution_fiber);

  long long done_compiling_glsl = emscripten_get_now();
  long long done_compiling_shader = -1;
  bool is_animated;
  const uint8_t *shader_source;
  if (status == JANET_SIGNAL_OK) {
    if (janet_checktype(response, JANET_TUPLE)) {
      const Janet *tuple = janet_unwrap_tuple(response);
      is_animated = janet_unwrap_boolean(tuple[0]);
      shader_source = janet_unwrap_string(tuple[1]);
      done_compiling_shader = emscripten_get_now();
    } else if (janet_checktype(response, JANET_KEYWORD)) {
      // either an error during compilation, or it was
      // passed an invalid value
      return compilation_error("invalid value");
    } else {
      janet_eprintf("unexpected compilation result %p\n", response);
      return compilation_error("internal error");
    }
  } else {
    janet_stacktrace(execution_fiber, response);
    return compilation_error("compilation error");
  }

  if (done_compiling_shader == -1) {
    printf("eval: %lldms diff: %lldms\n", (done_evaluating - start_time), (done_compiling_glsl - done_evaluating));
  } else {
    printf("eval: %lldms generate glsl: %lldms compile shader: %lldms\n",
      (done_evaluating - start_time),
      (done_compiling_glsl - done_evaluating),
      (done_compiling_shader - done_compiling_glsl));
  }

  return (CompilationResult) {
   .is_error = false,
   .shader_source = string((const char *)shader_source),
   .is_animated = is_animated,
   .error = ""
  };
}

using namespace emscripten;

EMSCRIPTEN_BINDINGS(module) {
  value_object<CompilationResult>("CompilationResult")
    .field("isError", &CompilationResult::is_error)
    .field("shaderSource", &CompilationResult::shader_source)
    .field("isAnimated", &CompilationResult::is_animated)
    .field("error", &CompilationResult::error)
    ;

  function("evaluate_script", &evaluate_script, allow_raw_pointers());
};
