#include "parse.h"
#include "sc.h"
#include "ginstr.h"
#include "vm.h"
#include <fmt/core.h>
#include <iostream>
#include <fstream>

static inline
bool match_arg(char const *arg, char const *pat)
{
  while (*arg && *pat && *arg == *pat) {
    ++arg;
    ++pat;
  }

  return !*pat;
}

int main(int argc, char const **argv)
{
  std::string input_file;

  for (int i = 1; i != argc; ++i) {
    char const *arg = argv[i];
    if (match_arg(arg, "-d")) {
      gi::set_dump_steps(true);
    } else if (match_arg(arg, "-s")) {
      gi::add_dump_sc_name(arg + 2 /* skip -s */);
    } else {
      input_file = arg;
    }
  }

  std::string source;

  if (input_file.empty()) {
    for (std::string line; std::getline(std::cin, line); source += '\n')
      source += line;
  } else {
    auto input_fs = std::ifstream(input_file);
    if (!input_fs) {
      fmt::print(stderr, "Cannot open file [{}]", input_file);
      return 1;
    }

    for (std::string line; std::getline(input_fs, line); source += '\n')
      source += line;
  }

  int exit_code = 0;
  try {
    auto [stmts, entry] = gi::parse_instrs(source);
    auto sections       = gi::preprocess(stmts);
    exit_code           = gi::interp(sections, entry);
  } catch (std::exception const &e) {
    fmt::print(stderr, "Something went wront: {}", e.what());
    return 1;
  } catch (...) {
    fmt::print(stderr, "Something went wront...");
    return 1;
  }

  return exit_code;
}

