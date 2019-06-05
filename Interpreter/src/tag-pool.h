#pragma once
#include "prelude.h"
#include "misc.h"

namespace gi {

/**
 * 指令中许多名字是字符串,比较代价相对高,故在这里池化.
 */
struct tag_pool_s
{
  std::vector<std::unique_ptr<char []>>   pool;
  std::map<std::string, u32> index_by_tag_name;

  char const *assign(std::string const &raw_tag)
  {
    auto found = index_by_tag_name.find(raw_tag);
    if (found == index_by_tag_name.end()) {
      return new_tag(raw_tag);
    } else {
      auto index = found->second;
      return pool.at(index).get();
    }
  }

  char const *new_tag(std::string const &raw_tag)
  {
    auto raw_str = new char[raw_tag.size()];
    std::strcpy(raw_str, raw_tag.c_str());
    u32 idx = pool.size();
    pool.emplace_back(raw_str);
    index_by_tag_name.emplace(raw_str, idx);
    return raw_str;
  }
};


} // namespace gi
