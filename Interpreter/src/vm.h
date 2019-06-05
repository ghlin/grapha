#pragma once

#include "cell.h"

namespace gi {

struct vm_session_s;
struct program_s;
struct tag_pool_s;

struct vm_statistics_s
{
  u32     steps             = 0;
  u32     cells_peak        = 0;
  u32     cells_total       = 0;
  u32     gc_cycles         = 0;
  u32     gc_collected_objs = 0;
};

struct vm_eval_options_s
{
  bool            dump_root;
  bool            dump_instr;
  cell_ref_t     *stack_buffer;
  std::size_t     stack_size;
  cell_s         *heap_buffer;
  std::size_t     heap_size;
};

extern
vm_session_s    *vm_create_session( program_s               *program
                                  , tag_pool_s              *tag_pool
                                  , vm_eval_options_s const &options);

extern
cell_ref_t       vm_eval_program(vm_session_s *vm);

extern
vm_statistics_s  vm_get_statistics(vm_session_s *vm);

extern
void             vm_close_session(vm_session_s *vm);

} // namespace gi

