#include "parse.h"
#include <cctype>
#include <fmt/core.h>

namespace gi {

using pos_t = char const *;

static inline
pos_t skip_spaces( pos_t in
                 , pos_t last)
{
  while (in != last && std::isspace(*in))
    ++in;

  return in;
}

static inline
pos_t skip_until_eol( pos_t in
                    , pos_t last)
{
  while (in != last && *in != '\n')
    ++in;

  return in;
}

static inline
pos_t skip_line_comment( pos_t in
                       , pos_t last)
{
  auto p = skip_spaces(in, last);

  while (p != last && p[0] == ';')
  {
    p = skip_spaces(skip_until_eol(p + 1, last), last);
  }

  return p;
}

struct token_wrapper_s
{
  ginstr_name_s token;
};

token_wrapper_s next_token( pos_t *in
                          , pos_t  last)
{
  token_wrapper_s w;
  char *i = w.token;

  char const *p = *in;
  while (p != last && !isspace(*p))
    *i++ = *p++;

  *i = 0;

  *in = skip_line_comment(p, last);

  return w;
}

static inline
GInstrType probe_type(char const *name)
{
#define PROBE(s, t) if (std::strcmp(name, #s) == 0) return t
  PROBE(push,        GI_PushRef);
  PROBE(pushi,       GI_PushPrim);
  PROBE(pushglobal,  GI_PushGlobal);
  PROBE(builtin,     GI_Builtin);
  PROBE(pick,        GI_SelComp);
  PROBE(pack,        GI_MakeObj);
  PROBE(test,        GI_TestObj);
  PROBE(mkapp,       GI_MakeAppl);
  PROBE(update,      GI_Update);
  PROBE(pop,         GI_Pop);
  PROBE(slide,       GI_Slide);
  PROBE(alloc,       GI_Alloc);
  PROBE(label,       GI_Label);
  PROBE(jump,        GI_Jump);
  PROBE(jfalse,      GI_JumpFalse);
  PROBE(globalstart, GI_GlobalStart);
  PROBE(globalend,   GI_GlobalEnd);
  PROBE(unwind,      GI_Unwind);
  PROBE(entry,       GI_Entry);
#undef  PROBE
  fmt::print(stderr, "Unknown instruction {}\n", name);

  return GI_UNUSED; /* error */
}

static inline
void report_error(char const *msg)
{
  std::fprintf(stderr, "Parse error: %s\n", msg);
  throw std::runtime_error(msg);
}

static inline
void parse_instrs(GInstrs *instrs, Str *entry_name, pos_t in, pos_t last)
{
  if (in == last) return;

  auto next    = in;
  auto [token] = next_token(&next, last);

  GInstr instr;
  instr.t = probe_type(token);

  switch (instr.t) {
  case GI_MakeAppl: case GI_Unwind: case GI_GlobalEnd:
    break;

  case GI_PushRef:    case GI_PushPrim:    case GI_Update:
  case GI_Pop:        case GI_Slide:       case GI_Alloc:
  case GI_Label:      case GI_Jump:        case GI_JumpFalse:
  case GI_TestObj:    case GI_MakeObj:     case GI_SelComp:
  case GI_PushGlobal: case GI_Builtin:     case GI_GlobalStart:
  case GI_Entry:
  {
    auto no_more_tokens = next == last;
    auto [value] = next_token(&next, last);

    if (  instr.t == GI_PushGlobal  // name first.
       || instr.t == GI_Builtin
       || instr.t == GI_GlobalStart
       || instr.t == GI_TestObj
       || instr.t == GI_MakeObj
       || instr.t == GI_Entry)
    {
      if (no_more_tokens) {
        return report_error("Unexpected end of file, identifier expected.");
      }

      std::strcpy(instr.d.name, value);
    } else {
      if (no_more_tokens) {
        return report_error("Unexpected end of file, integer value expected.");
      }

      instr.d.value = std::atol(value);
    }

    if (  instr.t == GI_GlobalStart // two fields.
       || instr.t == GI_MakeObj
       || instr.t == GI_Builtin)
    {
      if (next == last) return report_error("Unexpected end of file, integer value expected.");

      auto [arity] = next_token(&next, last);
      instr.d.arity = std::atol(arity);
    }
    break;
  }
  default:
    std::fprintf(stderr, "instr: [%s]\n", token);
    return report_error("Unknown instruction");
  }

  if (instr.t == GI_Entry)
    *entry_name = instr.d.name;
  else
    instrs->push_back(instr);

  return parse_instrs(instrs, entry_name, next, last);
}

extern std::pair<GInstrs, Str> parse_instrs(Str const &source)
{
  GInstrs    out;
  Str        entry_name;

  auto start = source.c_str();
  auto last  = start + source.size();

  parse_instrs(&out, &entry_name, skip_line_comment(start, last), last);

  return { out, entry_name };
}

} // namespace gi
