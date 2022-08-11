#include <emscripten.h>
#include "janet.h"

#define GL_DEBUG

#include <emscripten.h>
#include <GLES2/gl2.h>
#include <EGL/egl.h>
#include <emscripten/html5.h>

static GLuint program = 0;
static GLuint current_fragment_shader = 0;
static JanetFiber *draw_fiber = NULL;

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

void init_gl() {
  EmscriptenWebGLContextAttributes attrs;
  emscripten_webgl_init_context_attributes(&attrs);
  attrs.antialias = 0;
  // attrs.alpha = 0;
  attrs.depth = 0;
  attrs.stencil = 0;
  attrs.majorVersion = 2;

  EMSCRIPTEN_WEBGL_CONTEXT_HANDLE context = emscripten_webgl_create_context("#render-target", &attrs);

  if (context <= 0) {
    fprintf(stderr, "failed to create context %d\n", context);
  }

  emscripten_webgl_make_context_current(context);

  program = glCreateProgram();

  char *vertex_shader_source =
    "#version 300 es\n"
    "in vec4 position;\n"
    "void main() {\n"
    "  gl_Position = position;\n"
    "}\n"
  ;

  GLuint vertexShader = compile_shader(GL_VERTEX_SHADER, vertex_shader_source);

  glAttachShader(program, vertexShader);
}

void draw_triangles() {
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

  GLint positionLoc = glGetAttribLocation(program, "position");

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
  FILE *file;
  char *text;
   
  file = fopen(filename, "r");
  fseek(file, 0L, SEEK_END);
  long length = ftell(file);
  fseek(file, 0L, SEEK_SET);

  text = (char *)calloc(length, sizeof(char));

  long bytes_read = 0;
  while (bytes_read < length) {
    bytes_read += fread(text, sizeof(char), length, file);
  }
  fclose(file);
  return text;
}

JANET_FN(set_fragment_shader, "(set-fragment-shader)", "") {
  janet_fixarity(argc, 1);
  const uint8_t *shader_source = janet_getstring(argv, 0);

  if (0) {
    printf("%s\n", shader_source);
  }

  if (current_fragment_shader == 0) {
    init_gl();
  } else {
    glDetachShader(program, current_fragment_shader);
    glDeleteShader(current_fragment_shader);
  }

  GLuint fragment_shader = compile_shader(GL_FRAGMENT_SHADER, (char *)shader_source);

  glAttachShader(program, fragment_shader);
  glLinkProgram(program);
  glUseProgram(program);

  draw_triangles();

  current_fragment_shader = fragment_shader;

  return janet_wrap_nil();
}

void initialize() {
  janet_init();
  JanetTable *env = janet_core_env(NULL);

  const JanetRegExt regs[] = {
    JANET_REG("set-fragment-shader", set_fragment_shader),
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

    // we want to resume it once here so that it's ready to be yielded to in the future
    Janet query;
    janet_continue(draw_fiber, result, &query);
  } else {
    fprintf(stderr, "unable to initialize\n");
    janet_deinit();
  }
}

EMSCRIPTEN_KEEPALIVE
int run_janet(char *source, float camera_x, float camera_y, float camera_zoom) {
  long long start_time = emscripten_get_now();

  if (draw_fiber == NULL) {
    initialize();
  }

  if (draw_fiber == NULL) {
    return 1;
  }

  JanetTable *env = janet_core_env(NULL);

  Janet result;
  JanetSignal status = janet_dostring(env, source, "playground", &result);

  long long done_evaluating = emscripten_get_now();

  JanetKV *camera_struct = janet_struct_begin(2 * 3);
  janet_struct_put(camera_struct, janet_ckeywordv("x"), janet_wrap_number(camera_x));
  janet_struct_put(camera_struct, janet_ckeywordv("y"), janet_wrap_number(camera_y));
  janet_struct_put(camera_struct, janet_ckeywordv("zoom"), janet_wrap_number(camera_zoom));
  Janet camera = janet_wrap_struct(janet_struct_end(camera_struct));

  Janet *arg_tuple = janet_tuple_begin(2);
  arg_tuple[0] = result;
  arg_tuple[1] = camera;
  Janet args = janet_wrap_tuple(janet_tuple_end(arg_tuple));

  if (status == JANET_SIGNAL_OK) {
    Janet query;
    JanetSignal status = janet_continue(draw_fiber, args, &query);
    if (status == JANET_SIGNAL_YIELD) {
      if (janet_checktype(query, JANET_NIL) == 0) {
        fprintf(stderr, "yielded a value?? that's bizarre.\n");
      }
    } else {
      fprintf(stderr, "Fiber did not yield\n");
      janet_stacktrace(draw_fiber, query);
    }
  } else {
    fprintf(stderr, "Error while evaluating value\n");
  }

  long long done_rendering = emscripten_get_now();

  printf("eval: %lldms render: %lldms\n", (done_evaluating - start_time), (done_rendering - done_evaluating));

  return status;
}
