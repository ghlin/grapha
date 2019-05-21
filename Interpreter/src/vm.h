#pragma once
#include "node.h"
#include "ginstr.h"
#include "sc.h"

namespace gi {

extern
ginstr_prim_t interp(SectionMap const &sections, Str const &entry_name);

extern
void set_dump_steps(bool dump_steps);

extern
void add_dump_sc_name(Str const &name);

} // namespace gi
