#pragma once
#include "ginstr.h"

namespace gi {

extern
std::pair<GInstrs, Str> // instrs & main entry
parse_instrs(Str const &source);

} // namespace gi
