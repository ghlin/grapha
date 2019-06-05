#include "instruction.h"
#include <fmt/core.h>

namespace gi {

char const *do_pretty_print_instr_type(instruction_type_t ty)
{
#define X(t) if (ty == t) { return &#t[3]; /* skip 'IT_' */ }
  X(IT_PUSH_REF);
  X(IT_PUSH_INT);
  X(IT_PUSH_GLOBAL);
  X(IT_BUILTIN);
  X(IT_PACK);
  X(IT_PICK);
  X(IT_TEST);
  X(IT_MKAPP);
  X(IT_UPDATE);
  X(IT_POP);
  X(IT_SLIDE);
  X(IT_ALLOC);
  X(IT_LABEL);
  X(IT_JUMP);
  X(IT_JUMP_FALSE);
  X(IT_UNWIND);
  X(IT_EVAL);
  X(IT_COMMENT);
  X(IT_ENTRY);
  X(IT_GLOBAL_START);
  X(IT_GLOBAL_END);
#undef  X

  return nullptr;
}

std::string bad_instr(instruction_type_t ty)
{
  return fmt::format("INVALID_INSTR({})", ty);
}

std::string pretty_print_instr_type(instruction_type_t ty)
{
  auto ppty = do_pretty_print_instr_type(ty);
  return ppty ?: bad_instr(ty);
}

extern
std::string pretty_print_instr(instruction_s const &instr)
{
  auto ppty = do_pretty_print_instr_type(instr.t);
  if (!ppty) return bad_instr(instr.t);

  switch (instr.t)
  {
  case IT_MKAPP: case IT_GLOBAL_END: case IT_UNWIND:
  case IT_EVAL:
    return ppty;
  case IT_PUSH_REF: case IT_PUSH_INT:   case IT_POP:
  case IT_SLIDE:    case IT_ALLOC:      case IT_UPDATE:
  case IT_JUMP:     case IT_JUMP_FALSE: case IT_LABEL:
  case IT_PICK:
    return fmt::format("{:<14} {}", ppty, instr.d.val);
  case IT_PUSH_GLOBAL: case IT_TEST: case IT_ENTRY:
    return fmt::format("{:<14} {}", ppty, instr.d.name);
  case IT_BUILTIN: case IT_PACK: case IT_GLOBAL_START:
    return fmt::format("{:<14} {:<3} {}", ppty, instr.d.arity, instr.d.name);
  default:
    return bad_instr(instr.t);
  }
}


} // namespace gi

