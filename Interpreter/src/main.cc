#include "parse.h"
#include "sc.h"
#include "ginstr.h"
#include "vm.h"
#include <fmt/core.h>
#include <iostream>

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
  std::string source;

  for (int i = 1; i != argc; ++i) {
    char const *arg = argv[i];
    if (match_arg(arg, "-d")) {
      gi::set_dump_steps(true);
    } else if (match_arg(arg, "-s")) {
      gi::add_dump_sc_name(arg + 2 /* skip -s */);
    } else {
      std::cerr << "Unknown arg [" << arg << "]" << std::endl;
    }
  }

  for (std::string line; std::getline(std::cin, line); source += '\n')
    source += line;

  try {
    auto [stmts, entry] = gi::parse_instrs(source);
    auto sections       = gi::preprocess(stmts);
    auto result         = gi::interp(sections, entry);

    std::cout << result << std::endl;
  } catch (std::exception const &e) {
    std::cerr << "Something went wrong: " << e.what() << std::endl;
    return 1;
  } catch (...) {
    std::cerr << "Something went wrong..." << std::endl;
    return 1;
  }

  return 0;
}

