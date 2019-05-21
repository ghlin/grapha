#include "node.h"
#include <fmt/core.h>

namespace gi {

static inline
char const *pretty_print_node_type(NodeType t)
{
#define pp(x) if (t == x) return &*(#x) /* &* to silent the warning */ + 2
  pp(N_Proc);
  pp(N_App);
  pp(N_Hole);
  pp(N_Prim);
  pp(N_Pack);
#undef  pp

  return "Unknown";
}

Str pretty_print_node(Node const &node)
{
  auto ppt = pretty_print_node_type(node.t);
  if (node.t == N_Proc) {
    return fmt::format("{} ({} arity={})", node.d.name, ppt, node.d.arity);
  }

  if (node.t == N_Prim) {
    return fmt::format("{} ({})", node.d.value, ppt);
  }

  return ppt;
}

Seq<Str> do_pretty_print_tree(node_ref_t root, Set<node_ref_t> &v);

Seq<Str> make_tree(Str const &heading, node_ref_t *children, Set<node_ref_t> &v)
{
  auto deco1  = " -+- ";
  auto cont1 = "  |  ";
  auto deco2 = "  `- ";
  auto cont2 = "     ";
  auto fills = Str(heading.size(), ' ');

  Seq<Str> lines;

  auto fill = heading.c_str();
  auto deco = deco1;

  while (*children) {
    auto join_lines = do_pretty_print_tree(*children, v);
    auto cont       = children[1] ? cont1 : cont2;

    for (auto const &join_line: join_lines) {
      lines.push_back(fill + (deco + join_line));

      fill = fills.c_str();
      deco = cont;
    }

    deco = deco2;
    ++children;
  }

  return lines;
}

Seq<Str> do_pretty_print_tree(node_ref_t root, Set<node_ref_t> &v)
{
  if (v.find(root) != v.end()) {
    return { "(∞)" };
  }
  v.insert(root);

  node_ref_t tmp[3];

  auto is_app = root->t == N_App;

  if (is_app) {
    tmp[0] = root->d.lhs;
    tmp[1] = root->d.rhs;
    tmp[2] = 0;
  }

  if (is_app || root->t == N_Pack) {
    node_ref_t *children = is_app ? tmp : root->d.fields;
    auto label = is_app ? Str("@ ") : fmt::format("pack({}) ", root->d.tag);
    return make_tree(label, children, v);
  }

  return { pretty_print_node(*root) };
}

Str pretty_print_tree(node_ref_t root, Str const &leading)
{
  Str  e;
  Str  l = leading + ' ';
  auto p = "\n" + Str(leading.size() + 1, ' ');
  Set<node_ref_t> v;
  auto s = do_pretty_print_tree(root, v);

  for (auto const &line: s) {
    e += l;
    l  = p;
    e += line;
  }
  return e;
}

} // namespace gi

