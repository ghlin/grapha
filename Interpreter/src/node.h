#pragma once
#include "prelude.h"
#include "ginstr.h"

namespace gi {

struct Node;
using node_ref_t = Node *;

enum NodeType { N_Hole    /* hole, for letrec */
              , N_Prim    /* primitive value, currently only INT */
              , N_Proc    /* supercombinator (procedure) */
              , N_Pack    /* packed object */
              , N_App     /* application */
              };

#define NODE_PACKED_OBJECT_FIELDS_MAX 10

struct NodeData {
  union {
    ginstr_prim_t   value;   /* N_Prim       */
    struct {                 /* N_App */
      Node         *lhs;
      Node         *rhs;
    };

    struct {                 /* N_Proc */
      char const    *name;
      u32            arity;
      GInstrs const *program;
    };

    struct {                 /* N_Pack */
      char const   *tag;
      node_ref_t    fields[NODE_PACKED_OBJECT_FIELDS_MAX + 1];
    };
  };
};

struct Node {
  NodeType t;
  NodeData d;
};

extern
Str pretty_print_node(Node const &node);

extern
Str pretty_print_tree(node_ref_t root, Str const &leading);

} // namespace gi

