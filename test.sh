#!/bin/sh
set -e
cd "$(dirname "$0")"

./build.sh "$@"

for f in examples/*.rsm; do
  echo "——————————————————————————————————————————————————————————————————————————"
  echo "rsm -r -R0=3 '$f'"
  out/rsm -r -R0=3 "$f"
done
