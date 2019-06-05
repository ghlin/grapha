#include "load.h"
#include "tag-pool.h"
#include "misc.h"

namespace gi {

#define MAX_TOKEN_LEN 256 // FIXME:

using token_buff_t = char [MAX_TOKEN_LEN];

instruction_type_t
parse_instr_type(char const *token)
{
#define X(s, t) if (std::strcmp(token, #s) == 0) return t
  X(pack,        IT_PACK);
  X(globalstart, IT_GLOBAL_START);
  X(builtin,     IT_BUILTIN);
  X(push,        IT_PUSH_REF);
  X(pick,        IT_PICK);
  X(test,        IT_TEST);
  X(pushi,       IT_PUSH_INT);
  X(pushglobal,  IT_PUSH_GLOBAL);
  X(update,      IT_UPDATE);
  X(pop,         IT_POP);
  X(slide,       IT_SLIDE);
  X(alloc,       IT_ALLOC);
  X(label,       IT_LABEL);
  X(jump,        IT_JUMP);
  X(jfalse,      IT_JUMP_FALSE);
  X(entry,       IT_ENTRY);
  X(globalend,   IT_GLOBAL_END);
  X(unwind,      IT_UNWIND);
  X(eval,        IT_EVAL);
  X(mkapp,       IT_MKAPP);
#undef X
  return IT_UNUSED;
}

bool
parse_instr( std::string const &line
           , tag_pool_s        *tag_pool
           , instruction_s     *instr)
{
  if (line.empty()) return false;

  auto src = line.c_str();
  token_buff_t token;

  if (src[0] == ';') return false;

  if (std::sscanf(src, "%s", token) != 1) {
    gi_log("Cannot parse: {}\n", src);
    gi_assert(false);
  }

  auto t = parse_instr_type(token);
  auto status = 0;

  switch (t)
  {
  case IT_PACK: case IT_GLOBAL_START: case IT_BUILTIN:
    status = std::sscanf(src, "%*s %s %d", token, &instr->d.arity);
    gi_assert(status == 2);
    break;
  case IT_PUSH_REF: case IT_UPDATE: case IT_POP:  case IT_SLIDE:
  case IT_ALLOC:    case IT_LABEL:  case IT_JUMP: case IT_JUMP_FALSE:
  case IT_PICK:
    status = std::sscanf(src, "%*s %u", &instr->d.n);
    gi_assert(status == 1);
    break;
  case IT_TEST: case IT_PUSH_GLOBAL: case IT_ENTRY:
    status = std::sscanf(src, "%*s %s", token);
    gi_assert(status == 1);
    break;
  case IT_PUSH_INT:
    status = std::sscanf(src, "%*s %d", &instr->d.val);
    gi_assert(status == 1);
    break;
  }

  if (  t == IT_PACK
     || t == IT_GLOBAL_START
     || t == IT_BUILTIN
     || t == IT_TEST
     || t == IT_PUSH_GLOBAL
     || t == IT_ENTRY)
  {
    instr->d.name = tag_pool->assign(token);
  }

  instr->t = t;

  return true;
}

using offset_map_s = std::map<u32, u32>;

offset_map_s map_offsets( std::vector<instruction_s> const &raw_instrs
                        , u32                               first)
{
  offset_map_s offsets;
  for ( u32 off = 0
      ; first != raw_instrs.size() && raw_instrs.at(first).t != IT_GLOBAL_END
      ; ++first)
  {
    auto &instr = raw_instrs.at(first);
    if (instr.t == IT_LABEL) offsets[instr.d.n] = off;
    else if (instr.t != IT_COMMENT) ++off;
  }
  return offsets;
}

u32 cache_label_offsets( std::vector<instruction_s> const &raw_instrs
                       , u32                               first
                       , proc_s                           *proc)
{
  auto &proc_instr = raw_instrs.at(first++); /* skip it */
  proc->name  = proc_instr.d.name;
  proc->arity = proc_instr.d.arity;

  for ( auto offsets = map_offsets(raw_instrs, first)
      ; first != raw_instrs.size() && raw_instrs.at(first).t != IT_GLOBAL_END
      ; ++first)
  {
    auto instr = raw_instrs.at(first);
    if (instr.t == IT_LABEL) continue;

    if (instr.t == IT_JUMP || instr.t == IT_JUMP_FALSE) {
      instr.d.off = offsets.at(instr.d.n);
    }

    if (instr.t != IT_COMMENT) {
      proc->instrs.push_back(instr);
    }
  }

  return first == raw_instrs.size() ? first : first + 1;
}

program_s
load_program(std::vector<instruction_s> const &raw_instrs)
{
  program_s program;
  u32 first = 0;

  while (first != raw_instrs.size()) {
    auto &instr = raw_instrs.at(first);
    if (instr.t == IT_GLOBAL_START) {
      proc_s proc;
      first = cache_label_offsets(raw_instrs, first, &proc);
      program.proc_table[proc.name] = std::move(proc);
    } else if (instr.t == IT_COMMENT) {
      ++first;
    } else {
      gi_assert(instr.t == IT_ENTRY);
      program.entry_proc_name = instr.d.name;
      ++first;
    }
  }

  return program;
}

} // namespace gi
