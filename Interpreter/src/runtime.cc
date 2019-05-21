#include "runtime.h"
#include "misc.h"

namespace gi {

node_ref_t interp_builtin(char const *name, u32 arity, node_ref_t *args, node_ref_t result)
{
#define CASE(x) else if (std::strcmp(name, #x) == 0)
#define DEFAULT else
#define SWITCH  if (false) { }
#define V(n)    (args[arity - (n)]->d.value)

  result->t = N_Prim;
  SWITCH
  CASE (==)  { result->d.value = V(1) == V(2); }
  CASE (-)   { result->d.value = V(1) -  V(2); }
  CASE (+)   { result->d.value = V(1) +  V(2); }
  CASE (*)   { result->d.value = V(1) *  V(2); }
  CASE (/)   { result->d.value = V(1) /  V(2); }
  CASE (undefined) {
    fmt::print(stderr, "Bottom\n");
    throw std::runtime_error("Bottom");
  }
  DEFAULT {
    fmt::print(stderr, "Unknown builtin instruction [{}, {}]\n", name, arity);
    throw std::runtime_error("Unknown builtin");
  }

  return result;
}


} // namespace gi

