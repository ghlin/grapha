#pragma once
#include "prelude.h"
#include "instruction.h"

namespace gi {

struct tag_pool_s;

extern bool
parse_instr(std::string const &line, tag_pool_s *tag_pool, instruction_s *instr);

struct proc_s
{
  char const                 *name;
  u32                         arity;
  std::vector<instruction_s>  instrs;
};

struct program_s
{
  std::map<char const *, proc_s>  proc_table;
  char const                     *entry_proc_name;
};

extern program_s
load_program(std::vector<instruction_s> const &raw_instrs);

} // namespace gi
