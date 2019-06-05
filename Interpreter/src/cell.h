#pragma once

#include "prelude.h"
#include "instruction.h"

namespace gi {

enum CellType { CT_HOLE
              , CT_PRIM
              , CT_PROC
              , CT_PACK
              , CT_APP
              };

using cell_type_t = u32;

struct cell_s;
struct proc_s;
struct cell_field_list_s;

using cell_ref_t = cell_s *;

struct cell_data_s
{
  union {
    i32             val;
    cell_s         *fun;
    char   const   *tag;
    char   const   *name;
  };

  union {
    cell_s            *arg;
    cell_field_list_s *pack;
    proc_s const      *proc;
  };
};

struct cell_s
{
  u32         t       :4;
  u32         gc_mark :4;
  u32         id      :24;
  cell_data_s d;
};

struct cell_field_list_s
{
  u32        arity;
  u32        ref_count = 0;
  cell_ref_t fields[];
};

extern std::string
pretty_print_cell_type(cell_type_t ty);

extern std::string
pretty_print_cell(cell_s const &cell);

extern std::string
pretty_print_tree(std::string const &leading, cell_ref_t root);

} // namespace gi
