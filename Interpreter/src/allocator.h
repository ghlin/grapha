#pragma once

#include "cell.h"

namespace gi {

struct vm_allocator_impl_s;

class vm_allocator_s;
struct vm_session_s;

struct vm_root_objects_s
{
  virtual ~vm_root_objects_s();
  virtual void push_root_objects(vm_allocator_s *) = 0;
};

class vm_allocator_s
{
  vm_allocator_impl_s *impl;

  vm_allocator_s &operator = (vm_allocator_s const&) = delete;
  vm_allocator_s(vm_allocator_s const &)             = delete;
  vm_allocator_s &operator = (vm_allocator_s &&)     = delete;
  vm_allocator_s(vm_allocator_s &&)                  = delete;
public:
  vm_allocator_s( cell_s            *heap_buffer
                , std::size_t        heap_size
                , vm_root_objects_s *root_objs);
  ~vm_allocator_s();

  cell_ref_t         acquire(cell_type_t ty);
  cell_ref_t         acquire_pack(char const *tag, u32 arity);
  cell_field_list_s *acquire_field_list(u32 arity);
  void               overwrite_cell(cell_ref_t dst, cell_s const *src);

  void               gc_push_root(cell_ref_t root);
  void               run_gc();

  std::size_t        gc_cycles()            const;
  std::size_t        gc_collected_objects() const;
};

} // namespace gi



