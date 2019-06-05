#pragma once
#include "prelude.h"

namespace gi {

/**
 * tags for instruction
 */
enum InstructionType { IT_UNUSED     /**! not used */
                     /***************/
                     , IT_PUSH_REF
                     , IT_PUSH_INT
                     , IT_PUSH_GLOBAL
                     , IT_BUILTIN
                     , IT_PACK
                     , IT_PICK
                     , IT_TEST
                     , IT_MKAPP
                     , IT_UPDATE
                     , IT_POP
                     , IT_SLIDE
                     , IT_ALLOC
                     , IT_LABEL
                     , IT_JUMP
                     , IT_JUMP_FALSE
                     , IT_UNWIND
                     , IT_EVAL
                     /***************/
                     , IT_PSEDU_START
                     , IT_COMMENT     = IT_PSEDU_START
                     , IT_ENTRY
                     , IT_GLOBAL_START
                     , IT_GLOBAL_END
                     /***************/
                     , IT_TYPE_MAX
                     };

using instruction_type_t = InstructionType;

struct instruction_data_s
{
  union {
    u32         ref;
    u32         n;   /* pop, slide, alloc */
    u32         off;
    i32         val;
    char const *tag;
    char const *name;
  };

  u32         arity;
};

struct instruction_s
{
  instruction_type_t t;
  instruction_data_s d;
};

extern
std::string pretty_print_instr_type(instruction_type_t ty);

extern
std::string pretty_print_instr(instruction_s const &instr);

} // namespace gi

