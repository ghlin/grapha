#pragma once
#include "ginstr.h"

namespace gi {

struct SC {
  Str       name;
  u32       arity;
  GInstrs   program;
};

using SectionMap = Map<Str, SC>;

extern
SectionMap preprocess(GInstrs const &instrs);

} // namespace gi
