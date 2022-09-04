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

static JanetFiber *draw_fiber = NULL;
static gl_context *global_context = NULL;

GLuint compile_shader(GLenum type, char *source) {
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
char *slurp(char *filename) {
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

gl_context *new_gl_context(const char*selector) {
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

  char *vertex_shader_source =
    "#version 300 es\n"
    "in vec4 position;\n"
    "void main() {\n"
    "  gl_Position = position;\n"
    "}\n";
  GLuint vertex_shader = compile_shader(GL_VERTEX_SHADER, vertex_shader_source);

  char *fragment_shader_source =
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
  gl_context *context = calloc(1, sizeof(*context));
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

  char *startup_source = slurp("shade.janet");

  Janet result;
  JanetSignal status = janet_dostring(env, startup_source, "shade.janet", &result);
  free(startup_source);

  if (status == JANET_SIGNAL_OK) {
    janet_gcroot(result);
    draw_fiber = janet_unwrap_fiber(result);
    global_context = new_gl_context("#render-target");

    // we want to resume it once here so that it's ready to be yielded to in the future
    Janet response;
    int status = janet_continue(draw_fiber, result, &response);
    if (status != JANET_SIGNAL_YIELD || !janet_checktype(response, JANET_NIL)) {
      janet_eprintf("fiber began in a weird state %d %p\n", status, response);
    }
  } else {
    fprintf(stderr, "unable to initialize\n");
    janet_deinit();
  }
}

EMSCRIPTEN_KEEPALIVE
void update_camera(float x, float y, float zoom) {
  mat3 camera_matrix = rotate_xy(x, y);
  vec3 camera_origin = { .v = { 0.0, 0.0, 256.0 * zoom } };
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
int evaluate_script(char *source) {
  long long start_time = emscripten_get_now();

  if (draw_fiber == NULL) {
    fprintf(stderr, "unable to initialize compilation fiber\n");
    return 1;
  }

  JanetTable *env = janet_core_env(NULL);

  Janet result;
  JanetSignal status = janet_dostring(env, source, "playground", &result);

  long long done_evaluating = emscripten_get_now();

  if (status != JANET_SIGNAL_OK) {
    return -status;
  }

  Janet response;
  status = janet_continue(draw_fiber, result, &response);
  long long done_compiling_glsl = emscripten_get_now();
  long long done_compiling_shader = -1;
  int to_return;
  if (status == JANET_SIGNAL_YIELD) {
    if (janet_checktype(response, JANET_NIL)) {
      to_return = 0;
      // value was identical; nothing to do
    } else if (janet_checktype(response, JANET_TUPLE)) {
      const Janet *tuple = janet_unwrap_tuple(response);
      int is_animated = janet_unwrap_boolean(tuple[0]);
      const uint8_t *source = janet_unwrap_string(tuple[1]);
      set_fragment_shader(global_context, source);
      done_compiling_shader = emscripten_get_now();
      to_return = is_animated ? 2 : 1;
    } else {
      to_return = -1;
      janet_eprintf("fiber yielded an unexpected value %p\n", response);
    }
  } else {
    fprintf(stderr, "compilation fiber did not yield\n");
    janet_stacktrace(draw_fiber, response);
    to_return = -status;
  }

  if (done_compiling_shader == -1) {
    printf("eval: %lldms diff: %lldms\n", (done_evaluating - start_time), (done_compiling_glsl - done_evaluating));
  } else {
    printf("eval: %lldms generate glsl: %lldms compile shader: %lldms\n",
      (done_evaluating - start_time),
      (done_compiling_glsl - done_evaluating),
      (done_compiling_shader - done_compiling_glsl));
  }

  // 0 => no change
  // 1 => recompiled; static image
  // 2 => recompiled; animated
  // any negative value => error
  return to_return;
}
