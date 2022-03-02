#!/bin/sh
set -e
if [ $1 = "-w" ]; then shift; exec autorun -no-banner "$1" -- "$0" "$@"; fi

mkdir -p out/sinspect
IF=$(basename "$1")
S=out/sinspect/$IF-verbatim.s
O=out/sinspect/$IF.o
S2=out/sinspect/$IF.s
S3=out/sinspect/$IF-objdump.s
B=out/sinspect/$IF-stripped-prev.s
D=out/sinspect/$IF.s.diff
PF=out/sinspect/$IF.sh

echo "$S2"

[ -x /usr/local/opt/llvm/bin/clang ] && export PATH=/usr/local/opt/llvm/bin:$PATH
[ -x /opt/homebrew/opt/llvm/bin/clang ] && export PATH=/opt/homebrew/opt/llvm/bin:$PATH
CCDEF=cc ; command -v clang >/dev/null && CCDEF=clang
CC=${CC:-$CCDEF}

# copy previous, compile new
[ -f $S2 ] && cp $S2 $B
$CC -Oz -std=c11 -g -S -o $S "$@" &
$CC -Oz -std=c11 -g -c -o $O "$@"
wait

OBJDUMP=$(command -v clang)
[ -n "$OBJDUMP" ] && OBJDUMP=$(dirname "$OBJDUMP")/objdump
[ -x "$OBJDUMP" ] && "$OBJDUMP" -S --no-show-raw-insn -l $O > $S3

eval "$(cat "$PF" 2>/dev/null)" || true

IGN_PAT='^(?:\s*[;#]|\s+\.)'
LABEL_PAT='^\.?[0-9A-Za-z_]+:'
ARCH=$(uname -m)
case "$(command -v $CC)" in
  */clang) ARCH=$(clang -print-effective-triple "$@" | cut -d- -f1) ;;
esac

case "$ARCH" in
arm64)  BR_PAT='^\s*(?:B(?:\.\w+)?|Bcc|BLR|CBN?Z|TBN?Z)\s+[^_]' ;; # excludes "BR"
x86_64) BR_PAT='^\s*J[A-LN-Z][A-Z]*\b' ;; # excludes "JMP"
*)
  echo "don't know how to find stats for $ARCH" >&2
  exit 1
esac

grep -Ev "$IGN_PAT" $S > $S2  # strip comments

NBYTE=$(stat -f %z $S2)
NINSTR=$(grep -Ev "$LABEL_PAT" $S2 | wc -l | awk '{print $1}')
NLABEL=$(grep -E "$LABEL_PAT" $S2 | wc -l | awk '{print $1}')
NBR=$(grep -Ei "$BR_PAT" $S2 | wc -l | awk '{print $1}')

cat <<END > "$PF"
PREV_NBYTE=$NBYTE
PREV_NINSTR=$NINSTR
PREV_NBR=$NBR
PREV_NLABEL=$NLABEL
END

if [ -f $B ]; then
  # if [ -z "$DIFF_FILTER" ]; then
  #   DIFF_FILTER=/usr/share/git-core/contrib/diff-highlight
  #   if command -v brew >/dev/null; then
  #     DIFF_FILTER=$(brew --prefix git)/share/git-core/contrib/diff-highlight/diff-highlight
  #   fi
  #   [ -x "$DIFF_FILTER" ] || DIFF_FILTER=cat
  # fi
  # diff -w -d -q $B $S2 >/dev/null ||
  #   diff -U 1 -w -d --suppress-common-lines $B $S2 > $D || cat $D | $DIFF_FILTER
  diff -U 1 -w -d --suppress-common-lines $B $S2 > $D || true
fi

# ANSI colors: (\e[3Nm or \e[9Nm) 1 red, 2 green, 3 yellow, 4 blue, 5 magenta, 6 cyan
_report() {
  local LABEL=$1;shift
  local DELTA=$(( $1 - ${2:-$1} ))
  local ICON="—"
  printf "%-12s %6d" "$LABEL" $1
  (( $DELTA > 0 )) && printf "\e[91m" && ICON="▲"
  (( $DELTA < 0 )) && printf "\e[92m" && ICON="▼"
  (( $DELTA == 0 )) && printf "\e[2m"
  printf " %4d %s\e[0m\n" $DELTA "$ICON"
}

_report "instructions" $NINSTR $PREV_NINSTR
_report "branches"     $NBR    $PREV_NBR
_report "labels"       $NLABEL $PREV_NLABEL
_report "file size"    $NBYTE  $PREV_NBYTE
