#!/bin/sh
set -euo pipefail
cd "$(dirname "$0")"

OUTDIR=out/test
RSM=$OUTDIR/rsm

_x() { echo "$@"; "$@"; }

rm -rf "$OUTDIR"
_x ./build.sh -out="$OUTDIR" "$@" -- rsm rsm.wasm

for srcfile in examples/*.rsm; do
  echo "——————————————————————————————————————————————————————————————————————————"
  FILENAME=${srcfile##*/}

  # skip files matching "fail-*.rsm"
  case "$FILENAME" in
    fail-*) continue ;;
  esac

  # skip source files containing "//!exe2-only"
  grep -qE "^\/\/\!exe2-only" "$srcfile" && continue

  ROM=$OUTDIR/test_${FILENAME%*.rsm}.rom

  _x $RSM -o "$ROM" "$srcfile"

  # Note: null stdin to avoid read on stdin blocking the test script
  _x $RSM -R0=3 "$ROM" </dev/null
done

echo "——————————————————————————————————————————————————————————————————————————"
_x node test-wasm.mjs
