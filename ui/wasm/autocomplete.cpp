#include <string>
#include <stdio.h>
#include "janet.h"
#include "autocomplete.h"
#include "util.h"

  using std::string;

Definition unsafe_parse_definition(const Janet *definition) {
  return (Definition) {
    std::string(reinterpret_cast<const char *>(janet_unwrap_string(definition[0]))),
    std::string(reinterpret_cast<const char *>(janet_unwrap_string(definition[1]))),
    std::string(reinterpret_cast<const char *>(janet_unwrap_string(definition[2]))),
    janet_unwrap_integer(definition[3]),
  };
}

std::vector<Definition> get_definitions(JanetFunction *janetfn_get_definitions) {
  Janet result;
  if (!call_fn(janetfn_get_definitions, 0, NULL, &result)) {
    janet_panicf("could not get definitions");
  }

  JanetArray *definitions = janet_unwrap_array(result);
  int32_t count = definitions->count;

  auto definition_vec = std::vector<Definition>();

  for (int32_t i = 0; i < count; i++) {
    definition_vec.push_back(unsafe_parse_definition(janet_unwrap_tuple(definitions->data[i])));
  }

  return definition_vec;
}
