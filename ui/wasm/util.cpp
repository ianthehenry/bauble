#include <stdio.h>
#include <assert.h>
#include "janet.h"
#include "util.h"

Janet env_lookup(JanetTable *env, const char *name) {
  Janet entry = janet_table_get(env, janet_csymbolv(name));
  if (!janet_checktype(entry, JANET_TABLE)) {
    janet_panicf("environment entry %s missing", name);
  }
  return janet_table_get(janet_unwrap_table(entry), janet_ckeywordv("value"));
}

JanetFunction *env_lookup_function(JanetTable *env, const char *name) {
  Janet value = env_lookup(env, name);
  if (!janet_checktype(value, JANET_FUNCTION)) {
    janet_panicf("expected %s to be a function, got %q\n", name, value);
  }
  return janet_unwrap_function(value);
}

bool call_fn(JanetFunction *fn, int argc, const Janet *argv, Janet *out) {
  JanetFiber *fiber = NULL;
  if (janet_pcall(fn, argc, argv, out, &fiber) == JANET_SIGNAL_OK) {
    return true;
  } else {
    janet_stacktrace(fiber, *out);
    return false;
  }
}

unsigned char *read_file(const char *filename, size_t *length) {
  size_t capacity = 2 << 17;
  unsigned char *src = (unsigned char *)malloc(capacity * sizeof(unsigned char));
  assert(src);
  size_t total_bytes_read = 0;
  FILE *file = fopen(filename, "r");
  assert(file);
  size_t bytes_read;
  do {
    size_t remaining_capacity = capacity - total_bytes_read;
    if (remaining_capacity == 0) {
      capacity <<= 1;
      src = (unsigned char *)realloc(src, capacity * sizeof(unsigned char));
      assert(src);
      remaining_capacity = capacity - total_bytes_read;
    }

    bytes_read = fread(&src[total_bytes_read], sizeof(unsigned char), remaining_capacity, file);
    total_bytes_read += bytes_read;
  } while (bytes_read > 0);

  fclose(file);
  *length = total_bytes_read;
  return src;
}
