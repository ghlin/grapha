#include "sc.h"
#include "misc.h"

namespace gi {

using OffsetMap = Map<ginstr_ref_t, size_t>;

using instr_ptr_t = GInstrs::const_iterator;

static inline
OffsetMap map_offset( instr_ptr_t first /* skip the GI_GlobalEnd */
                    , instr_ptr_t last)
{
  OffsetMap result;

  for (size_t off = 0; first != last && first->t != GI_GlobalEnd; ++first) {
    if (first->t == GI_Label) {
      result[first->d.label] = off;
    } else ++off;
  }

  return result;
}

static inline
SC next_section( instr_ptr_t *in
               , instr_ptr_t  last)
{
  auto iter = *in;

  gi_assert(iter->t == GI_GlobalStart);

  SC result;
  result.name  = iter->d.name;
  result.arity = iter->d.arity;

  for (auto by_label = map_offset(++iter, last); iter != last && iter->t != GI_GlobalEnd; ++iter) {
    if (iter->t == GI_Label) continue;

    if (iter->t == GI_Jump || iter->t == GI_JumpFalse) {
      auto instr = *iter;
      instr.d.offset = by_label.at(instr.d.label);
      result.program.emplace_back(instr);
    } else {
      result.program.emplace_back(*iter);
    }
  }

  *in = iter == last ? iter : iter + 1; /* skip GI_GlobalEnd */

  return result;
}

extern
SectionMap preprocess(GInstrs const &instrs)
{
  SectionMap result;
  auto first = instrs.cbegin();
  auto last  = instrs.cend();

  while (first != last) {
    auto sc = next_section(&first, last);
    result[sc.name] = std::move(sc);
  }

  return result;
}

} // namespace gi
