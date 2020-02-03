# Grapha

A functional programming language, follow the book _The implementation of functional programming language_ and _Functional programming language: a tutorial_.

# How to build

install `stack`.

install `cmake`.

install `cxxopts` & `libfmt`.

## build the compiler

``` bash
cd <project-root>
stack install

# try it
grc -h
```

## build the interpreter

```
cd <anywhere-u-like>
mkdir build
cd    build
cmake <project-root>/Interpreter  -DCMAKE_INSTALL_PREFIX=~/.local
make
make install

# try it
gi
```

# The compiler

```
grc - Grapha compiler

Usage: grc FILE [-o|--output-file ARG] [-t|--skip-typecheck] [-p|--no-prelude]
           [-c|--dump-core] [-s|--dump-sc]
  The Grapha compiler

Available options:
  -o,--output-file ARG     output file name
  -t,--skip-typecheck      don't run type checker
  -p,--no-prelude          don't inject the prelude
  -c,--dump-core           instead of generating GCode, dump corecombinators
  -s,--dump-sc             instead of generating GCode, dump supercombinators
  -h,--help                Show this help text
```

# The interpreter

```
GCode instruction interpreter
Usage:
  gi [OPTION...] FILE

  -i, --input FILE       input file
  -s, --stack-size SIZE  stack size (in bytes)
  -p, --heap-size SIZE   heap size (in bytes)
  -r, --report-stat      report statistic
  -e, --dump-root        dump root cell
  -t, --dump-instr       dump instr
```

# run demos

``` bash
cd <project-root>/examples

./compile-and-run.sh    <example>.txt
```

