#include "cell.h"
#include "load.h"
#include <fmt/core.h>

namespace gi {

static inline char const *
do_pretty_print_cell_type(cell_type_t ty)
{
#define X(x) if (ty == x) return &#x[3]
  X(CT_APP);
  X(CT_HOLE);
  X(CT_PRIM);
  X(CT_PROC);
  X(CT_PACK);
#undef X
  return nullptr;
}

static inline std::string
bad_cell(cell_type_t ty)
{
  return fmt::format("INVALID_CELL({})", ty);
}

std::string
pretty_print_cell_type(cell_type_t ty)
{
  auto ppty = do_pretty_print_cell_type(ty);
  return ppty ?: bad_cell(ty);
}

std::string
pretty_print_cell(cell_s const &cell)
{
  auto ppty = do_pretty_print_cell_type(cell.t);
  if (cell.t == CT_PROC || cell.t == CT_PACK) {
    return fmt::format( "[#{:<05}] {} ({} arity = {})"
                      , cell.id
                      , ppty
                      , cell.d.name
                      , cell.t == CT_PACK ? cell.d.pack ? cell.d.pack->arity : 0 : cell.d.proc->arity);
  }
  if (cell.t == CT_PRIM) {
    return fmt::format("[#{:<05}] {} ({})", cell.id, ppty, cell.d.val);
  }

  return fmt::format("[#{:<05}] {}({})", cell.id, ppty, (void *)&cell);
}

static inline
std::vector<std::string>
do_pretty_print_tree(cell_s *root, std::set<u32> &visited);

static inline
std::vector<std::string>
make_tree( std::string const &leading
         , u32                arity
         , cell_ref_t        *fields
         , std::set<u32>     &visited)
{
  if (arity == 0) return { leading };

  char const *joint_1 = " -*- ";
  char const *joint_2 = "  `- ";
  char const *vertbar = "  ¦  ";
  char const *fillbar = "     ";

  auto fills = std::string(leading.size(), ' ');

  std::vector<std::string> graph;

  auto fill = leading.c_str();
  auto deco = joint_1;

  for (u32 i = 0; i != arity; ++i) {
    auto join_lines = do_pretty_print_tree(fields[i], visited);
    auto bar        = i + 1 == arity ? fillbar : vertbar;

    for (auto const &join_line: join_lines) {
      graph.push_back(fill + (deco + join_line));
      fill = fills.c_str();
      deco = bar;
    }

    deco = joint_2;
  }

  return graph;
}

std::vector<std::string>
do_pretty_print_tree(cell_s *root, std::set<u32> &visited)
{
  if (visited.find(root->id) != visited.end()) {
    return { fmt::format("[#{:>05}] (∞)", unsigned(root->id)) };
  }
  visited.insert(root->id);

  cell_ref_t app_fields[2];
  auto is_app = root->t == CT_APP;

  if (is_app) {
    app_fields[0] = root->d.fun;
    app_fields[1] = root->d.arg;
  }

  if (is_app || root->t == CT_PACK) {
    auto fields  = is_app ? app_fields : root->d.pack ? root->d.pack->fields : nullptr;
    auto arity   = is_app ? 2 : root->d.pack ? root->d.pack->arity : 0;
    auto label   = is_app ? "@" : fmt::format("pack({})", root->d.tag);
    auto leading = fmt::format("[#{:>05}] {}", unsigned(root->id), label);
    return make_tree(leading, arity, fields, visited);
  }

  return { pretty_print_cell(*root) };
}

std::string pretty_print_tree(std::string const &leading, cell_ref_t root)
{
  std::set<u32> visited;
  auto tree = do_pretty_print_tree(root, visited);

  std::string e;
  std::string l = leading + ' ';
  std::string p = "\n" + std::string(leading.size() + 1, ' ');

  auto *c = &l;
  for (auto const &line: tree) {
    e += *c;
    e += line;
    c  = &p;
  }

  return e;
}

} // namespace gi

