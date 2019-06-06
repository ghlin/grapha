#!/bin/sh

TARGET="$1"

echo "Target: $TARGET"
bat $TARGET

echo "Compiling: $TARGET"

([ "$TARGET" -nt "$TARGET.compiled" ] \
  && grc $TARGET -o $TARGET.compiled  \
  || echo "Already compiled.") \
  && echo "Compiled $TARGET => $TARGET.compiled" \
  && gi  $TARGET.compiled -r

