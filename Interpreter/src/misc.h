#pragma once

#include <fmt/core.h>

#define gi_assert(expr...)                         \
  do { if (expr) { } else {                        \
    fmt::print( stderr                             \
              , "Assert error: {}\n"               \
                " In file      {}\n"               \
                "    function  {}\n"               \
                "    line      {}\n"               \
              , #expr                              \
              , __FILE__                           \
              , __PRETTY_FUNCTION__                \
              , __LINE__);                         \
    throw std::runtime_error("Assertion failed");  \
  } } while (false)

#define gi_log(expr...)  fmt::print(stderr, expr)
#define gi_dump_exp(exp) gi_log("{} ::\t" #exp "\t=>\t{}\n", __func__, exp)
