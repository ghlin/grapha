#include "vm.h"
#include "cell.h"
#include "tag-pool.h"
#include "allocator.h"
#include "instruction.h"
#include "load.h"
#include "misc.h"

namespace gi {

#define dump_cell(cell) fmt::print(stderr, "{}\n", pretty_print_tree(#cell " =", cell))

// {{{ the runtime stack
struct vm_stack_s
{
  cell_ref_t *base;
  u32         next = 0;

  cell_ref_t top()
  {
    gi_assert(next != 0);
    return base[next - 1];
  }

  cell_ref_t pop()
  {
    gi_assert(next != 0);
    return base[--next];
  }

  void pop(u32 n)
  {
    gi_assert(next >= n);
    next -= n;
  }

  void push(cell_ref_t p)
  {
    base[next++] = p;
  }

  cell_ref_t r(u32  i)
  {
    gi_assert(i < next);
    return base[next - i - 1];
  }

  cell_ref_t at(u32 i)
  {
    gi_assert(i < next);
    return base[i];
  }

  void update(u32 i, cell_ref_t new_cell)
  {
    gi_assert(i < next);
    base[next - i - 1] = new_cell;
  }

  cell_ref_t *peek(u32 n)
  {
    try {
      gi_assert(n <= next);
    } catch (...) {
      fmt::print(stderr, "next = {}, n = {}\n", next, n);
      throw;
    }
    return base + next - n;
  }

  vm_stack_s fork()
  {
    return vm_stack_s { base + next, 0 };
  }

  void dump() const
  {
    gi_log("==============[ dump start ]=============\n");
    gi_log("base = {}, next = {}\n", (void *)base, next);
    for (u32 i = 0; i != next; ++i) {
      gi_log("{}\n", pretty_print_tree(fmt::format("{:>2}", i), base[i]));
    }
  }
};
// }}}

static inline
cell_ref_t find_redux(cell_ref_t root)
{
  auto left_most = root;
  u32  depth     = 0;
  while (left_most->t == CT_APP) {
    left_most = left_most->d.fun;
    ++depth;
  }

  if (left_most->t != CT_PROC) {
    // WHNF
    return nullptr;
  }

  auto arity = left_most->d.proc->arity;

  if (depth < arity) {
    // WHNF
    return nullptr;
  }

  auto through = depth - arity;
  auto redux   = root;
  while (through--)
    redux = redux->d.fun;

  return redux;
}

static inline
cell_ref_t unwind_stack(vm_stack_s *s, cell_ref_t spine)
{
  if (!spine) return nullptr;

  for (s->push(spine); spine->t == CT_APP; spine = spine->d.fun) {
    s->push(spine->d.arg);
  }

  gi_assert(spine->t == CT_PROC);

  return spine;
}

struct unsolved_cell_s
{
  cell_ref_t root;
  cell_ref_t spine;
};

struct vm_frame_s
{
  u32                          depth;
  u32                          pc;
  proc_s const                *proc;
  vm_stack_s                   stack;

  std::vector<unsolved_cell_s> unwind_cells;
  std::vector<unsolved_cell_s> eval_cells;
};

struct vm_tags_s
{
  char const *int_eq;
  char const *int_neq;
  char const *int_lt;
  char const *int_gt;
  char const *int_lte;
  char const *int_gte;
  char const *int_pls;
  char const *int_mns;
  char const *int_mul;
  char const *int_div;
  char const *int_mod;
  char const *int_neg;
  char const *put_char;
  char const *get_char;
  char const *put_int;
  char const *get_int;
  char const *undefined;
  char const *seq;
  char const *from_io;

  char const *tru;
  char const *fls;
  char const *unit;
};

vm_tags_s initial_tags(tag_pool_s *tag_pool)
{
  vm_tags_s tags;

  tags.int_eq    = tag_pool->assign("==");
  tags.int_neq   = tag_pool->assign("!=");
  tags.int_lt    = tag_pool->assign("<");
  tags.int_gt    = tag_pool->assign(">");
  tags.int_lte   = tag_pool->assign("<=");
  tags.int_gte   = tag_pool->assign(">=");
  tags.int_pls   = tag_pool->assign("+");
  tags.int_mns   = tag_pool->assign("-");
  tags.int_mul   = tag_pool->assign("*");
  tags.int_div   = tag_pool->assign("/");
  tags.int_mod   = tag_pool->assign("%");
  tags.int_neg   = tag_pool->assign("neg");
  tags.put_char  = tag_pool->assign("put-char");
  tags.get_char  = tag_pool->assign("get-char");
  tags.put_int   = tag_pool->assign("put-int");
  tags.get_int   = tag_pool->assign("get-int");
  tags.undefined = tag_pool->assign("undefined");
  tags.seq       = tag_pool->assign("seq");
  tags.from_io   = tag_pool->assign("from-io");

  tags.fls       = tag_pool->assign("False");
  tags.tru       = tag_pool->assign("True");
  tags.unit      = tag_pool->assign("()");

  return tags;
}

struct vm_session_root_objects_s: vm_root_objects_s
{
  vm_session_s *vm;

  virtual void push_root_objects(vm_allocator_s *allocator) override;
};


struct vm_session_s
{
  program_s                     *program;
  tag_pool_s                    *tag_pool;
  vm_allocator_s                *allocator;
  std::vector<vm_frame_s>        frames;
  cell_ref_t                     last_step_result = nullptr;
  vm_tags_s                      tags;
  vm_statistics_s                stat;
  vm_eval_options_s              opts;
  vm_session_root_objects_s     *root_objs;

  proc_s const *lookup_proc(char const *name) const
  {
    auto found = program->proc_table.find(name);
    if (found == program->proc_table.end()) {
      fmt::print(stderr, "Unknown proc: {}\n", name);
      throw std::runtime_error("unknown proc");
    }
    return &found->second;
  }
};

void vm_session_root_objects_s::push_root_objects(vm_allocator_s *allocator)
{
  if (vm->last_step_result) {
    allocator->gc_push_root(vm->last_step_result);
  }

  for (auto &frame: vm->frames) {
    for (auto [root, spine]: frame.eval_cells) {
      allocator->gc_push_root(root);
      allocator->gc_push_root(spine);
    }
    for (auto [root, spine]: frame.unwind_cells) {
      allocator->gc_push_root(root);
      allocator->gc_push_root(spine);
    }
    for (u32 i = 0; i != frame.stack.next; ++i) {
      allocator->gc_push_root(frame.stack.base[i]);
    }
  }
}

#define maybe_unused              __attribute__((unused))
#define INSTR_HANDLER_NAME(instr) interp_##instr
#define HANDLE_INSTR(instr)       void INSTR_HANDLER_NAME(instr) ( maybe_unused vm_session_s const       *vm  \
                                                                 , maybe_unused vm_stack_s               *s   \
                                                                 , maybe_unused u32                      *ppc \
                                                                 , maybe_unused instruction_type_t        t   \
                                                                 , maybe_unused instruction_data_s const &d)

using instr_handler_t = void (*) ( vm_session_s const       *
                                 , vm_stack_s               *
                                 , u32                      *
                                 , instruction_type_t
                                 , instruction_data_s const &
                                 );

HANDLE_INSTR(IT_PUSH_REF)
{
  s->push(s->r(d.n));
}

HANDLE_INSTR(IT_PUSH_INT)
{
  auto prim = vm->allocator->acquire(CT_PRIM);
  prim->d.val = d.val;
  s->push(prim);
}

HANDLE_INSTR(IT_PUSH_GLOBAL)
{
  auto cell = vm->allocator->acquire(CT_PROC);
  cell->d.proc = vm->lookup_proc(d.name);
  cell->d.name = d.name;
  s->push(cell);
}

HANDLE_INSTR(IT_MKAPP)
{
  auto app = vm->allocator->acquire(CT_APP);
  app->d.fun = s->pop();
  app->d.arg = s->pop();
  s->push(app);
}

HANDLE_INSTR(IT_UPDATE)
{
  vm->allocator->overwrite_cell(s->r(d.n), s->top());
  s->update(d.n, s->top());
  s->pop();
}

HANDLE_INSTR(IT_POP)
{
  s->pop(d.n);
}

HANDLE_INSTR(IT_SLIDE)
{
  auto top = s->pop();
  s->pop(d.n);
  s->push(top);
}

HANDLE_INSTR(IT_ALLOC)
{
  for (u32 i = 0; i != d.n; ++i) {
    s->push(vm->allocator->acquire(CT_HOLE));
  }
}

HANDLE_INSTR(IT_JUMP)
{
  *ppc = d.off;
}

HANDLE_INSTR(IT_JUMP_FALSE)
{
  auto cond = s->pop();
  gi_assert(cond->t == CT_PACK);
  if (cond->d.tag[0] == 'F') /* False */ {
    *ppc = d.off;
  }
}

HANDLE_INSTR(IT_PACK)
{
  auto pack = vm->allocator->acquire_pack(d.tag, d.arity);
  auto args = s->peek(d.arity);

  for (u32 i = 0; i != d.arity; ++i) {
    pack->d.pack->fields[d.arity - i - 1] = args[i];
  }

  s->pop(d.arity);
  s->push(pack);
}

HANDLE_INSTR(IT_TEST)
{
  auto pack   = s->pop();

  gi_assert(pack->t == CT_PACK);

  auto tag    = pack->d.tag == d.tag ? vm->tags.tru : vm->tags.fls;
  auto result = vm->allocator->acquire_pack(tag, 0);

  s->push(result);
}

HANDLE_INSTR(IT_PICK)
{
  auto pack = s->pop();

  gi_assert(pack->t == CT_PACK);
  gi_assert(pack->d.pack != nullptr);
  gi_assert(pack->d.pack->arity > d.n);

  s->push(pack->d.pack->fields[d.n]);
}

HANDLE_INSTR(IT_BUILTIN)
{
  auto args = s->peek(d.arity);
  s->pop(d.arity);

#define MATCH(x)  (d.tag == vm->tags.x)
#define arg(n)    (args[d.arity - n])
#define argval(n) (arg(n)->d.val)

  auto result = vm->allocator->acquire(CT_HOLE);

  if (MATCH(int_eq)) {
    result->t      = CT_PACK;
    result->d.tag  = (argval(1) == argval(2)) ? vm->tags.tru : vm->tags.fls;
    result->d.pack = nullptr;
  } else if (MATCH(int_neq)) {
    result->t      = CT_PACK;
    result->d.tag  = (argval(1) != argval(2)) ? vm->tags.tru : vm->tags.fls;
    result->d.pack = nullptr;
  } else if (MATCH(int_lt)) {
    result->t      = CT_PACK;
    result->d.tag  = (argval(1) < argval(2)) ? vm->tags.tru : vm->tags.fls;
    result->d.pack = nullptr;
  } else if (MATCH(int_gt)) {
    result->t      = CT_PACK;
    result->d.tag  = (argval(1) > argval(2)) ? vm->tags.tru : vm->tags.fls;
    result->d.pack = nullptr;
  } else if (MATCH(int_lte)) {
    result->t      = CT_PACK;
    result->d.tag  = (argval(1) <= argval(2)) ? vm->tags.tru : vm->tags.fls;
    result->d.pack = nullptr;
  } else if (MATCH(int_gte)) {
    result->t      = CT_PACK;
    result->d.tag  = (argval(1) >= argval(2)) ? vm->tags.tru : vm->tags.fls;
    result->d.pack = nullptr;
  } else if (MATCH(int_mns)) {
    result->t      = CT_PRIM;
    result->d.val  = argval(1) - argval(2);
  } else if (MATCH(int_pls)) {
    result->t      = CT_PRIM;
    result->d.val  = argval(1) + argval(2);
  } else if (MATCH(int_mul)) {
    result->t      = CT_PRIM;
    result->d.val  = argval(1) * argval(2);
  } else if (MATCH(int_div)) {
    result->t      = CT_PRIM;
    result->d.val  = argval(1) / argval(2);
  } else if (MATCH(int_mod)) {
    result->t      = CT_PRIM;
    result->d.val  = argval(1) % argval(2);
  } else if (MATCH(int_neg)) {
    result->t      = CT_PRIM;
    result->d.val  = -argval(1);
  } else if (MATCH(get_char)) {
    result->t      = CT_PRIM;
    result->d.val  = std::getchar();
  } else if (MATCH(get_int)) {
    result->t      = CT_PRIM;
    std::scanf("%d", &result->d.val);
  } else if (MATCH(put_char)) {
    std::putchar(argval(1));
    result->t      = CT_PACK;
    result->d.pack = nullptr;
    result->d.tag  = vm->tags.unit;
  } else if (MATCH(put_int)) {
    std::printf("%d", argval(1));
    result->t      = CT_PACK;
    result->d.pack = nullptr;
    result->d.tag  = vm->tags.unit;
  } else if (MATCH(seq)) {
    vm->allocator->overwrite_cell(result, arg(2));
  } else if (MATCH(from_io)) {
    vm->allocator->overwrite_cell(result, arg(1));
  } else if (MATCH(undefined)) {
    throw std::runtime_error("«eval undefined»");
  } else {
    gi_log("UNIMPLEMENTED: {}\n", d.name);
    throw std::runtime_error("UNIMPLEMENTED BUILTIN");
  }
#undef MATCH
#undef arg
#undef argval

  s->push(result);
}

static instr_handler_t handlers[] = { nullptr /* IT_UNUSED */
                                    , INSTR_HANDLER_NAME(IT_PUSH_REF)
                                    , INSTR_HANDLER_NAME(IT_PUSH_INT)
                                    , INSTR_HANDLER_NAME(IT_PUSH_GLOBAL)
                                    , INSTR_HANDLER_NAME(IT_BUILTIN)
                                    , INSTR_HANDLER_NAME(IT_PACK)
                                    , INSTR_HANDLER_NAME(IT_PICK)
                                    , INSTR_HANDLER_NAME(IT_TEST)
                                    , INSTR_HANDLER_NAME(IT_MKAPP)
                                    , INSTR_HANDLER_NAME(IT_UPDATE)
                                    , INSTR_HANDLER_NAME(IT_POP)
                                    , INSTR_HANDLER_NAME(IT_SLIDE)
                                    , INSTR_HANDLER_NAME(IT_ALLOC)
                                    , nullptr /* IT_LABEL */
                                    , INSTR_HANDLER_NAME(IT_JUMP)
                                    , INSTR_HANDLER_NAME(IT_JUMP_FALSE)
                                    , nullptr /* IT_UNWIND */
                                    , nullptr /* IT_EVAL */
                                    , nullptr /* IT_COMMENT */
                                    , nullptr /* IT_ENTRY   */
                                    , nullptr /* IT_GLOBAL_START */
                                    , nullptr /* IT_GLOBAL_END   */
                                    };

auto prepare_frame( cell_ref_t  root
                  , vm_frame_s *this_frame
                  , vm_frame_s *next_frame)
{
  auto spine = find_redux(root);
  if (!spine) return spine;

  next_frame->stack = this_frame->stack;
  next_frame->pc    = 0;

  auto proc_cell   = unwind_stack(&next_frame->stack, spine);
  next_frame->proc = proc_cell->d.proc;

  return spine;
}

/**
 * true for return, false for grow
 */
bool exec_frame( vm_session_s *vm
               , vm_frame_s   *this_frame
               , vm_frame_s   *next_frame)
{
  while (this_frame->pc != this_frame->proc->instrs.size()) {
    vm->stat.steps++;

    auto &[t, d] = this_frame->proc->instrs.at(this_frame->pc);

    gi_assert(static_cast<u32>(t) < IT_TYPE_MAX);

    if (vm->opts.dump_instr) {
      this_frame->stack.dump();
      gi_log( "[{}] proc = {}, pc = {}/{}, instr = {}\n\n"
            , this_frame->depth
            , this_frame->proc->name
            , this_frame->pc
            , this_frame->proc->instrs.size()
            , pretty_print_instr({ t, d }));
    }

    if (t == IT_EVAL) {
      auto root = this_frame->stack.top();

      if (auto spine = prepare_frame(this_frame->stack.top(), this_frame, next_frame)) {
        this_frame->eval_cells.push_back({ root, spine });
        return false;
      } else {
        if (!this_frame->eval_cells.empty()) {
          auto [last_root, last_spine] = this_frame->eval_cells.back();
          this_frame->eval_cells.pop_back();
          this_frame->stack.pop();
          this_frame->stack.push(last_root);
          vm->allocator->overwrite_cell(last_spine, root);
        } else {
          this_frame->pc++;
        }
      }
    } else if (t == IT_UNWIND) {
      auto root = this_frame->stack.pop();

      if (auto spine = prepare_frame(root, this_frame, this_frame)) {
        this_frame->unwind_cells.push_back({ root, spine });
      } else {
        if (!this_frame->unwind_cells.empty()) {
          auto [last_root, last_spine] = this_frame->unwind_cells.back();
          this_frame->unwind_cells.pop_back();
          this_frame->stack.push(last_root);
          vm->allocator->overwrite_cell(last_spine, root);
        } else {
          this_frame->pc++;
        }
      }
    } else {
      auto handle = handlers[static_cast<u32>(t)];
      gi_assert(handle);

      this_frame->pc++;
      handle(vm, &this_frame->stack, &this_frame->pc, t, d);
    }
  }

  return true;
}

vm_frame_s initial_frame(vm_session_s *vm, cell_ref_t *runtime_stack_memory)
{
  auto proc = vm->lookup_proc(vm->program->entry_proc_name);
  runtime_stack_memory[0] = vm->allocator->acquire(CT_HOLE);
  return { 0, 0, proc, { runtime_stack_memory, 1 } };
}

cell_ref_t step_vm(vm_session_s *vm)
{
  vm_frame_s next_frame;
  auto &this_frame = vm->frames.back();
  if (exec_frame(vm, &this_frame, &next_frame)) {
    auto result = this_frame.stack.top();
    vm->frames.pop_back();
    if (!vm->frames.empty()) {
      vm->allocator->overwrite_cell(vm->frames.back().stack.top(), result);
    }
    return result;
  } else {
    next_frame.depth = vm->frames.size();
    vm->frames.push_back(next_frame);
    return nullptr;
  }
}

void exec_vm(vm_session_s *vm)
{
  vm->frames.push_back(initial_frame(vm, vm->opts.stack_buffer));
  while (!vm->frames.empty()) {
    if (vm->opts.dump_root) {
      auto root = vm->frames.front().stack.top();
      dump_cell(root);
    }
    vm->last_step_result = step_vm(vm);
  }
}

vm_session_s *vm_create_session( program_s               *program
                               , tag_pool_s              *tag_pool
                               , vm_eval_options_s const &options)
{
  auto vm = new vm_session_s;

  vm->opts          = options;
  vm->tag_pool      = tag_pool;
  vm->program       = program;
  vm->tags          = initial_tags(tag_pool);
  vm->root_objs     = new vm_session_root_objects_s;
  vm->root_objs->vm = vm;
  vm->allocator     = new vm_allocator_s( options.heap_buffer
                                    , options.heap_size
                                    , vm->root_objs);

  return vm;
}

vm_statistics_s  vm_get_statistics(vm_session_s *vm)
{
  return vm->stat;
}

cell_ref_t vm_eval_program(vm_session_s *vm)
{
  exec_vm(vm);

  vm->stat.gc_cycles         = vm->allocator->gc_cycles();
  vm->stat.gc_collected_objs = vm->allocator->gc_collected_objects();

  return vm->last_step_result;
}

void vm_close_session(vm_session_s *vm)
{
  delete vm->allocator;
  delete vm->root_objs;
  delete vm;
}

} // namespace gi

