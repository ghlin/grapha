#pragma once
#include "prelude.h"

namespace gi {

#define GI_MAX_NAME_LEN 32

using ginstr_ref_t   = u32;
using ginstr_prim_t  = i32;
using ginstr_label_t = u32;
using ginstr_name_s  = char [GI_MAX_NAME_LEN]; /* TODO: use char const * */

enum GInstrType { GI_UNUSED          /* unused */
                , GI_TYPE_BEGIN
                , GI_PushRef       = GI_TYPE_BEGIN
                , GI_PushPrim
                , GI_PushGlobal
                , GI_MakeAppl
                , GI_Update
                , GI_Pop
                , GI_Slide
                , GI_Alloc
                , GI_Label
                , GI_Jump
                , GI_JumpFalse
                , GI_TestObj
                , GI_MakeObj
                , GI_SelComp
                , GI_Builtin
                , GI_Unwind
                , GI_GlobalStart
                , GI_GlobalEnd
                , GI_Entry
                , GI_TYPE_END
                };

struct GInstrData {
  union {
    ginstr_ref_t   ref;     /** push, update */

    ginstr_prim_t  value;   /** pushprim */

    u32            n;       /** pop, slide, alloc */

    ginstr_label_t label;   /** label, jump, jfalse */
    size_t         offset;  /** label, jump, jfalse (translated) */

    u32            field;   /** selcomp */

    struct {
      union { ginstr_name_s name;   /** globalstart, pushglobal, builtin */
              ginstr_name_s tag;    /**  makeobj, selcomp, testobj       */
            };
      u32                 arity;    /** globalstart, makeobj, builtin     */
    };
  };
};

struct GInstr {
  GInstrType t; /** the type */
  GInstrData d; /** the data */
};

using GInstrs = Seq<GInstr>;

extern
char const *pretty_print_instr_type(GInstrType t);

extern
Str pretty_print_instr(GInstr const &instr);

} // namespace gi

