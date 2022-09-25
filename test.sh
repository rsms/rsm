#!/bin/sh
set -e
cd "$(dirname "$0")"

OUTDIR=out/test-$(uname -s)-$(uname -m)
RSM=$OUTDIR/rsm

./build.sh -out="$OUTDIR" "$@"

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

  echo "rsm -o '$ROM' '$srcfile'"
  $RSM -o "$ROM" "$srcfile"

  # Note: null stdin to avoid read on stdin blocking the test script
  echo "rsm -R0=3 '$ROM'"
  $RSM -R0=3 "$ROM" </dev/null
done
