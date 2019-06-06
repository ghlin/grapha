#!/bin/sh

TARGET="$1"

echo "Target: $TARGET"
bat $TARGET
echo "Compiling: $TARGET"

grc $TARGET -o $TARGET.compiled               && \
  echo "Compiled $TARGET => $TARGET.compiled" && \
  gi  $TARGET.compiled -r

