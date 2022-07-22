#!/bin/sh
set -e
cd "$(dirname "$0")"

./build.sh "$@"

for f in examples/*.rsm; do
  echo "——————————————————————————————————————————————————————————————————————————"
  ROM=${f##*/} ; ROM=out/test_${ROM%*.rsm}.rom

  echo "rsm -o '$ROM' '$f'"
  out/rsm -o "$ROM" "$f"

  # Note: null stdin to avoid read on stdin blocking the test script
  echo "rsm -R0=3 '$ROM'"
  out/rsm -R0=3 "$ROM" </dev/null
done
