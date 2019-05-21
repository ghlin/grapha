#include "vm.h"
#include "misc.h"
#include "runtime.h"

namespace gi {

static bool     g_dump_steps    = false;
static Seq<Str> g_dump_sc_names;

extern void set_dump_steps(bool dump_steps)   { g_dump_steps = dump_steps;       }
extern void add_dump_sc_name(Str const &name) { g_dump_sc_names.push_back(name); }

static inline
bool should_dump_sc(char const *name)
{
  for (auto &dump_sc: g_dump_sc_names) {
    if (std::strcmp(dump_sc.c_str(), name) == 0) return true;
  }
  return false;
}

struct NodeAllocator {
  Seq<Ptr<Node>>   pool;

  node_ref_t acquire(NodeType t)
  {
    auto n = std::make_unique<Node>(Node { t, {} });
    auto p = n.get();
    pool.emplace_back(std::move(n));
    return p;
  }

  node_ref_t makeapp(node_ref_t *stack)
  {
    auto app = acquire(N_App);
    app->d.lhs = stack[1];
    app->d.rhs = stack[0];
    return app;
  }

  node_ref_t makeobj(char const *tag, u32 arity, node_ref_t *fields)
  {
    auto pack = acquire(N_Pack);
    pack->d.tag = tag;
    for (u32 i = 0; i != arity; ++i)
      pack->d.fields[arity - i - 1] = fields[i];
    pack->d.fields[arity] = nullptr;
    return pack;
  }

  node_ref_t makeproc(char const *name, u32 arity, GInstrs const *program)
  {
    auto proc = acquire(N_Proc);

    proc->d.name    = name;
    proc->d.arity   = arity;
    proc->d.program = program;

    return proc;
  }
};


struct Stack {
  node_ref_t *base;
  u32         size = 0;

  node_ref_t top() {
    gi_assert(size != 0);
    return base[size - 1];
  }

  node_ref_t pop() {
    gi_assert(size != 0);
    return base[--size];
  }

  void pop(u32 n) {
    gi_assert(size >= n);
    size -= n;
  }

  void push(node_ref_t p) {
    base[size++] = p;
  }

  node_ref_t r(u32  i) {
    gi_assert(i < size);
    return base[size - i - 1];
  }

  node_ref_t at(u32 i) {
    gi_assert(i < size);
    return base[i];
  }

  node_ref_t *peek(u32 n) {
    try {
      gi_assert(n <= size);
    } catch (...) {
      fmt::print(stderr, "size = {}, n = {}\n", size, n);
      throw;
    }
    return base + size - n;
  }
};

void dump_stack(Stack *s)
{
  fmt::print(stderr, "==== stack dump ====\n");
  for (u32 i = 0; i != s->size; ++i) {
    fmt::print(stderr, "{}\n", pretty_print_tree(s->at(i), fmt::format("{:>2}", i)));
  }
}


static inline
node_ref_t find_redux(node_ref_t root)
{
  auto left_most = root;
  u32  depth = 0;
  while (left_most->t == N_App) {
    left_most = left_most->d.lhs;
    ++depth;
  }

  gi_assert(left_most->t == N_Proc);

  auto arity   = left_most->d.arity;
  auto through = depth - arity;
  auto redux   = root;

  while (through--)
    redux = redux->d.lhs;

  return redux;
}

static inline
node_ref_t unwind(Stack *s)
{
  auto root       = s->pop();
  auto walk       = find_redux(root);
  node_ref_t proc = nullptr;

  for (s->push(walk); walk->t == N_App; walk = walk->d.lhs) {
    s->push(walk->d.rhs);
    proc = walk->d.lhs;
  }

  return proc;
}

#define RUNTIME_STACK_SIZE (2048 * 10 * 10)

struct Frame
{
  char const     *sc_name;
  size_t          pc;
  GInstrs const  *program;
};

struct Context
{
  SectionMap const &sections;
  node_ref_t        runtime_stack[RUNTIME_STACK_SIZE];
  NodeAllocator     allocator;
  Seq<Frame>        frames;

  size_t            steps = 0;

  SC const &lookup(char const *name)
  {
    auto found = sections.find(name);
    if (found == sections.cend()) {
      fmt::print(stderr, "lookup: sc [{}] not found.\n", name);
      throw std::runtime_error("unknown sc");
    }

    return found->second;
  }

  void push_frame(char const *sc_name, GInstrs const *program)
  {
    frames.push_back({ sc_name, 0, program });
  }
};

#define HANDLE(name) void interp_##name( __attribute__((unused)) Context          *c   \
                                       , __attribute__((unused)) Stack            *s   \
                                       , __attribute__((unused)) size_t           *ppc \
                                       , __attribute__((unused)) GInstrType        t   \
                                       , __attribute__((unused)) GInstrData const &d   \
                                       )

using interp_func_t = void (*)( Context          *ctx
                              , Stack            *s
                              , size_t           *ppc
                              , GInstrType        t
                              , GInstrData const &d
                              );


node_ref_t interp(Context *ctx, Stack *s, char const *sc_name, GInstrs const *program);

HANDLE(GI_PushRef)
{
  s->push(s->r(d.n));
}

HANDLE(GI_PushPrim)
{
  auto prim = c->allocator.acquire(N_Prim);
  prim->d.value = d.value;
  s->push(prim);
}

HANDLE(GI_PushGlobal)
{
  auto &sc = c->lookup(d.name);

  s->push(c->allocator.makeproc(sc.name.c_str(), sc.arity, &sc.program));
}

HANDLE(GI_Unwind)
{
  auto target = s->top();
  if (target->t == N_Hole) {
    throw std::runtime_error("Attempt to evaluate a hole node");
  } else if (   target->t == N_Prim
            ||  target->t == N_Pack
            || (target->t == N_Proc && target->d.arity != 0)) {
    return;
  } else if (target->t == N_Proc) /* of zero arity */ {
    auto result = interp(c, s, target->d.name, target->d.program); // the stack was unchanged.
    if (s->top() != result) std::memcpy(s->top(), result, sizeof *result);
  } else if (target->t == N_App) {
    auto proc = unwind(s);

    gi_assert(proc->t == N_Proc);

    auto result = interp(c, s, proc->d.name, proc->d.program);
    if (s->top() != result) std::memcpy(s->top(), result, sizeof *result);
  } else {
    throw std::runtime_error("what happened?");
  }
}

HANDLE(GI_MakeAppl)
{
  auto app = c->allocator.makeapp(s->peek(2));
  s->pop(2);
  s->push(app);
}

HANDLE(GI_Update)
{
  if (d.n == 0) return;
  std::memcpy(s->r(d.n), s->top(), sizeof (Node));
  s->pop();
}

HANDLE(GI_Pop)
{
  s->pop(d.n);
}

HANDLE(GI_Slide)
{
  auto save = s->pop();
  s->pop(d.n);
  s->push(save);
}

HANDLE(GI_Alloc)
{
  for (u32 i = 0; i != d.n; ++i) {
    s->push(c->allocator.acquire(N_Hole));
  }
}

HANDLE(GI_Jump)
{
  *ppc = d.offset;
}

HANDLE(GI_JumpFalse)
{
  auto cond = s->pop();
  gi_assert(cond->t == N_Prim);
  if (cond->d.value == 0) *ppc = d.offset;
}

HANDLE(GI_MakeObj)
{
  auto pack = c->allocator.makeobj(d.tag, d.arity, s->base + s->size - d.arity);
  s->pop(d.arity);
  s->push(pack);
}

HANDLE(GI_TestObj)
{
  auto pack = s->pop();
  auto prim = c->allocator.acquire(N_Prim);

  prim->d.value = std::strcmp(pack->d.tag, d.tag) == 0;
  s->push(prim);
}

HANDLE(GI_SelComp)
{
  auto pack = s->pop();
  auto field = pack->d.fields[d.field];
  s->push(field);
}

HANDLE(GI_Builtin)
{
  auto args = s->peek(d.arity);
  s->pop(d.arity);
  auto result = interp_builtin( d.name
                              , d.arity
                              , args
                              , c->allocator.acquire(N_Prim)
                              );
  s->push(result);
}

#undef  HANDLE
#define HANDLE(name) &interp_##name

// MIND THE ORDER!!!
static interp_func_t handlers[] = {
  nullptr, /* GI_UNUSED */
  HANDLE(GI_PushRef),
  HANDLE(GI_PushPrim),
  HANDLE(GI_PushGlobal),
  HANDLE(GI_MakeAppl),
  HANDLE(GI_Update),
  HANDLE(GI_Pop),
  HANDLE(GI_Slide),
  HANDLE(GI_Alloc),
  nullptr, /* GI_Label */
  HANDLE(GI_Jump),
  HANDLE(GI_JumpFalse),
  HANDLE(GI_TestObj),
  HANDLE(GI_MakeObj),
  HANDLE(GI_SelComp),
  HANDLE(GI_Builtin),
  HANDLE(GI_Unwind),
  nullptr, /* GI_GlobalStart */
  nullptr, /* GI_GlobalEnd   */
  nullptr  /* GI_Entry       */
};

node_ref_t interp(Context *ctx, Stack *s, char const *sc_name, GInstrs const *program)
{
  size_t pc = 0, eop = program->size();
  while (pc != eop) {
    auto &[t, d] = program->at(pc);

    if (g_dump_steps || should_dump_sc(sc_name)) {
      dump_stack(s);
      gi_log("[{}] pc = {:>2} instr = {}\n", sc_name, pc, pretty_print_instr({ t, d }));
      gi_log("\n");
    }

    ++ctx->steps;
    ++pc;

    gi_assert(static_cast<int>(t) < GI_TYPE_END);
    gi_assert(static_cast<int>(t) > 0);
    gi_assert(t != GI_Label);
    gi_assert(t != GI_GlobalStart);
    gi_assert(t != GI_GlobalEnd);

    auto handle = handlers[t];

    gi_assert(handle != nullptr);

    handle(ctx, s, &pc, t, d);
  }

  return s->top();
}

ginstr_prim_t interp(SectionMap const &sections, Str const &entry_name)
{
  Context ctx = { sections, {}, {} };
  Stack   s   = { ctx.runtime_stack, 0 };
  auto   &sc  = ctx.lookup(entry_name.c_str());

  s.push(ctx.allocator.acquire(N_Hole));

  auto r = interp(&ctx, &s, entry_name.c_str(), &sc.program);

  fmt::print(stderr, "-- reduce steps:     {}\n", ctx.steps);
  fmt::print(stderr, "-- memory allocated: {} cell(s), {} kb\n", ctx.allocator.pool.size(), ctx.allocator.pool.size() * sizeof (Node) / 1024);
  fmt::print(stderr, "{}\n", pretty_print_tree(r, "-- interp returned: "));

  gi_assert(r->t == N_Prim);

  return r->d.value;
}

} // namespace gi

