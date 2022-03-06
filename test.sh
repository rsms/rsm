#!/bin/sh
set -e
cd "$(dirname "$0")"

./build.sh "$@"

for f in examples/*.rsm; do
  echo "——————————————————————————————————————————————————————————————————————————"
  ROM=${f%*.rsm}.rom

  echo "rsm -o '$ROM' '$f'"
  out/rsm -o "$ROM" "$f"

  echo "rsm -R0=3 '$ROM'"
  out/rsm -R0=3 "$ROM"
done
