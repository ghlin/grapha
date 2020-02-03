#include <queue>
#include "misc.h"
#include "allocator.h"

namespace gi {

vm_root_objects_s::~vm_root_objects_s() { }

union vm_cell_block_s
{
  cell_s cell;
  struct
  {
    u32              _;
    vm_cell_block_s *prev;
    vm_cell_block_s *next;
  };
};

static_assert(sizeof (vm_cell_block_s) == sizeof (cell_s));

struct vm_allocator_impl_s
{
  u32                     next_id = 1;

  std::size_t             heap_size;
  vm_cell_block_s        *heap_buffer;

  vm_cell_block_s         free_blocks_head;
  std::size_t             bound = 0;


  std::queue<cell_ref_t>  mark_q;
  vm_root_objects_s      *root_objs;

  u32                     total_collected = 0;
  u32                     gc_cycles       = 0;

  void initialize()
  {
    free_blocks_head.next = &free_blocks_head;
    free_blocks_head.prev = &free_blocks_head;
  }

  void free_cell(vm_cell_block_s *block)
  {
    block->cell.gc_mark = 2;

    gi_assert (block >= heap_buffer && block < heap_buffer + heap_size);

    auto next = free_blocks_head.next;
    auto prev = &free_blocks_head;

    block->next = next;
    block->prev = prev;

    next->prev = block;
    prev->next = block;
  }

  cell_ref_t try_alloc_from_free_blocks()
  {
    if (free_blocks_head.next == &free_blocks_head) {
      return nullptr;
    }

    auto first = free_blocks_head.next;

    auto prev = first->prev;
    auto next = first->next;

    prev->next = next;
    next->prev = prev;

    return &first->cell;
  }

  cell_ref_t try_alloc_from_buffer()
  {
    if (bound == heap_size) {
      return nullptr;
    }
    return &(heap_buffer + bound++)->cell;
  }

  cell_ref_t try_alloc()
  {
    return try_alloc_from_free_blocks() ?: try_alloc_from_buffer();
  }

  void mark_objects()
  {
    while (!mark_q.empty()) {
      auto target = mark_q.front(); mark_q.pop();
      if (target->gc_mark) continue;
      target->gc_mark = 1;
      if (target->t == CT_APP) {
        mark_q.push(target->d.fun);
        mark_q.push(target->d.arg);
      } else if (target->t == CT_PACK && target->d.pack) {
        for (u32 i = 0; i != target->d.pack->arity; ++i) {
          mark_q.push(target->d.pack->fields[i]);
        }
      }
    }
  }

  void scan_objects()
  {
    for (u32 i = 0; i != bound; ++i) {
      auto block = heap_buffer + i;
      auto cell = reinterpret_cast<cell_ref_t>(block);
      if (cell->gc_mark == 1) {
        cell->gc_mark = 0;
      } else if (cell->gc_mark == 0) {
        free_cell(block);
        ++total_collected;
      }
    }
  }

  void run_gc(vm_allocator_s *self)
  {
    gi_assert(mark_q.empty());

    ++gc_cycles;

    root_objs->push_root_objects(self);
    mark_objects();
    scan_objects();
  }

  cell_ref_t alloc(vm_allocator_s *self)
  {
    auto cell = try_alloc();
    if (!cell) {
      run_gc(self);
      cell = try_alloc();
    }
    if (!cell) {
      throw std::runtime_error("Out of memory");
    }
    cell->gc_mark = 0;
    return cell;
  }
};

vm_allocator_s::vm_allocator_s( cell_s            *heap_buffer
                              , std::size_t        heap_size
                              , vm_root_objects_s *root_objs)
  : impl(new vm_allocator_impl_s)
{
  impl->heap_buffer = reinterpret_cast<vm_cell_block_s *>(heap_buffer);
  impl->heap_size   = heap_size;
  impl->root_objs   = root_objs;
  impl->initialize();
}

vm_allocator_s::~vm_allocator_s() { delete impl; }

cell_ref_t         vm_allocator_s::acquire(cell_type_t ty)
{
  auto cell = impl->alloc(this);

  cell->t       = ty;
  cell->id      = impl->next_id++;
  cell->gc_mark = 0;

  return cell;
}

cell_ref_t         vm_allocator_s::acquire_pack(char const *tag, u32 arity)
{
  auto cell    = acquire(CT_PACK);
  cell->d.pack = acquire_field_list(arity);
  cell->d.tag  = tag;
  return cell;
}

cell_field_list_s *vm_allocator_s::acquire_field_list(u32 arity)
{
  if (arity == 0) return nullptr;
  auto list   = static_cast<cell_field_list_s *>(std::malloc(8 + sizeof (cell_s) * arity));
  list->arity = arity;
  return list;
}

void               vm_allocator_s::overwrite_cell(cell_ref_t dst, cell_s const *src)
{
  if (dst == src) return;
  dst->t = src->t;
  if (src->t == CT_PACK && src->d.pack) {
    src->d.pack->ref_count++;
  }
  if (dst->t == CT_PACK && dst->d.pack && dst->d.pack->ref_count != 0) {
    dst->d.pack->ref_count--;

    if (dst->d.pack->ref_count == 0) {
      std::free(dst->d.pack);
    }
  }
  std::memcpy(&dst->d, &src->d, sizeof src->d);
}

void               vm_allocator_s::gc_push_root(cell_ref_t root)
{
  impl->mark_q.push(root);
}

void               vm_allocator_s::run_gc()
{
  impl->run_gc(this);
}

std::size_t        vm_allocator_s::gc_cycles()            const
{
  return impl->gc_cycles;
}

std::size_t        vm_allocator_s::gc_collected_objects() const
{
  return impl->total_collected;
}


} // namespace gi
