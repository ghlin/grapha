#pragma once

#include "prelude.h"
#include "node.h"

namespace gi {

extern
node_ref_t interp_builtin( char const *name
                         , u32 arity
                         , node_ref_t *args
                         , node_ref_t  result);

} // namespace gi
