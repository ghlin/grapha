#include "ginstr.h"
#include <fmt/core.h>

namespace gi {

char const *pretty_print_instr_type(GInstrType t)
{
#define pp(x) if (t == x) return #x + 3
  pp(GI_PushRef);
  pp(GI_PushPrim);
  pp(GI_PushGlobal);
  pp(GI_MakeAppl);
  pp(GI_Update);
  pp(GI_Pop);
  pp(GI_Slide);
  pp(GI_Alloc);
  pp(GI_Label);
  pp(GI_Jump);
  pp(GI_JumpFalse);
  pp(GI_TestObj);
  pp(GI_MakeObj);
  pp(GI_SelComp);
  pp(GI_Builtin);
  pp(GI_GlobalStart);
  pp(GI_GlobalEnd);
  pp(GI_Unwind);
#undef  pp

  return "Unknown";
}

static inline
Str pretty_print_instr_data(GInstr const &instr)
{
  switch (instr.t) {
  case GI_PushRef:    case GI_PushPrim:    case GI_Update:
  case GI_Pop:        case GI_Slide:       case GI_Alloc:
  case GI_Label:      case GI_Jump:        case GI_JumpFalse:
    return fmt::format(" {}", instr.d.value);
  case GI_TestObj:    case GI_SelComp:
  case GI_PushGlobal: case GI_Builtin:     case GI_GlobalStart:
    return instr.t == GI_GlobalStart
      ? fmt::format(" {} {}", instr.d.name, instr.d.arity)
      : fmt::format(" {}", instr.d.name);
  case GI_MakeObj:
    return fmt::format(" {} {}", instr.d.tag, instr.d.arity);
  }

  return {};
}

extern
Str pretty_print_instr(GInstr const &instr)
{
  auto ppdata = pretty_print_instr_data(instr);
  return ppdata.empty()
    ? pretty_print_instr_type(instr.t)
    : fmt::format("{:<12} {}", pretty_print_instr_type(instr.t), ppdata);
}

} // namespace gi

