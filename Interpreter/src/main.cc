#include "instruction.h"
#include "tag-pool.h"
#include "cell.h"
#include "load.h"
#include "vm.h"
#include "misc.h"
#include <fmt/core.h>
#include <istream>
#include <fstream>
#include <cxxopts.hpp>

static inline
std::vector<gi::instruction_s>
load_from_stream(std::istream &source, gi::tag_pool_s *pool)
{
  std::vector<gi::instruction_s> raw_instrs;
  gi::instruction_s instr;
  for (std::string line; std::getline(source, line); ) {
    if (gi::parse_instr(line, pool, &instr))
      raw_instrs.push_back(instr);
  }
  return raw_instrs;
}

struct program_options_s
{
  std::string              input_file;
  std::size_t              stack_size    = 1024 * 32  * sizeof (gi::cell_s *);
  std::size_t              heap_size     = 1024 * 128 * sizeof (gi::cell_s);
  bool                     report_stat   = false;
  bool                     dump_root     = false;
  bool                     dump_instr    = false;
};

static inline
program_options_s
parse_cli(int argc, char **argv)
{
  program_options_s popts;

  auto options = cxxopts::Options(argv[0], "GCode instruction interpreter");

  options.add_options()
    ("i,input",       "input file",            cxxopts::value<std::string>(popts.input_file), "FILE")
    ("s,stack-size",  "stack size (in bytes)", cxxopts::value<std::size_t>(popts.stack_size), "SIZE")
    ("p,heap-size",   "heap size (in bytes)",  cxxopts::value<std::size_t>(popts.heap_size),  "SIZE")
    ("r,report-stat", "report statistic",      cxxopts::value<bool>(popts.report_stat))
    ("e,dump-root",   "dump root cell",        cxxopts::value<bool>(popts.dump_root))
    ("t,dump-instr",  "dump instr",            cxxopts::value<bool>(popts.dump_instr));

  options.parse_positional("input");
  options.positional_help("FILE");
  options.show_positional_help();
  auto result = options.parse(argc, argv);

  if (result.count("input") != 1) {
    fmt::print("{}\n", options.help());
  }

  return popts;
}

int main(int argc, char **argv)
{
  auto popts = parse_cli(argc, argv);

  auto input_file = std::ifstream(popts.input_file);
  if (!input_file) {
    fmt::print(stderr, "file [{}] cannot be opened.\n", popts.input_file);
    return 1;
  }

  gi::vm_eval_options_s ev_opts;

  auto stack_size   = (popts.stack_size - 1)  / sizeof (gi::cell_s *) + 1;
  auto heap_size    = (popts.heap_size - 1)   / sizeof (gi::cell_s)   + 1;

  auto stack_buffer = std::unique_ptr<gi::cell_s *[]>(new gi::cell_s *[stack_size]);
  auto heap_buffer  = std::unique_ptr<gi::cell_s []>(new gi::cell_s  [heap_size]);

  ev_opts.dump_root    = popts.dump_root;
  ev_opts.dump_instr   = popts.dump_instr;
  ev_opts.stack_buffer = stack_buffer.get();
  ev_opts.heap_buffer  = heap_buffer.get();
  ev_opts.stack_size   = stack_size;
  ev_opts.heap_size    = heap_size;

  if (popts.report_stat) {
    fmt::print("=========================================\n");
    fmt::print("Running program: {}\n", popts.input_file);
    fmt::print("Stack size {:>20} bytes, {:>19} cells\n", popts.stack_size, ev_opts.stack_size);
    fmt::print("Heap  size {:>20} bytes, {:>19} cells\n", popts.heap_size, ev_opts.heap_size);
    fmt::print("=========================================\n");
  }

  gi::tag_pool_s pool;

  auto raw_instrs = load_from_stream(input_file, &pool);
  auto program    = gi::load_program(raw_instrs);
  auto vm         = gi::vm_create_session(&program, &pool, ev_opts);
  auto cell       = gi::vm_eval_program(vm);

  if (popts.report_stat) {
    auto stat = gi::vm_get_statistics(vm);

    fmt::print("\n=========== STAT ==============\n");
    fmt::print("{}\n", gi::pretty_print_tree("result    =", cell));
    fmt::print("Steps     = {}\n", stat.steps);
    fmt::print("GC cycles = {}\n", stat.gc_cycles);
    fmt::print("GC objs   = {}\n", stat.gc_collected_objs);
    fmt::print("===============================\n");
  }

  gi::vm_close_session(vm);

  return 0;
}

