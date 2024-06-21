#include "janet.h"
#ifndef BAUBLE_UTIL_H
#define BAUBLE_UTIL_H
Janet env_lookup(JanetTable *env, const char *name);
JanetFunction *env_lookup_function(JanetTable *env, const char *name);
bool call_fn(JanetFunction *fn, int argc, const Janet *argv, Janet *out);
unsigned char *read_file(const char *filename, size_t *length);
#endif
