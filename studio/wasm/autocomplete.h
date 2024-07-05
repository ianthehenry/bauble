#include <string>
#include <stdio.h>
#include <vector>
#include "janet.h"
#ifndef BAUBLE_AUTOCOMPLETE_H
#define BAUBLE_AUTOCOMPLETE_H

using std::string;

struct Definition {
  string name;
  string args;
  string doc;
  int type; // 0 = value, 1 = function, 2 = macro
};

std::vector<Definition> get_definitions(JanetFunction *janetfn_get_definitions);
#endif
