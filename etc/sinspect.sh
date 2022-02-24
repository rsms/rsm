#!/bin/sh
set -e
if [ $1 = "-w" ]; then shift; exec autorun "$1" -- "$0" "$@"; fi

S=out/sinspect.s
B=out/sinspect-prev.s
D=out/sinspect.s.diff

[ -f $S ] && cp $S $B

clang -Oz -std=c11 -S -o $S "$@"

if [ -f $B ]; then
  if [ -z "$DIFF_FILTER" ]; then
    DIFF_FILTER=/usr/share/git-core/contrib/diff-highlight
    if command -v brew >/dev/null; then
      DIFF_FILTER=$(brew --prefix git)/share/git-core/contrib/diff-highlight/diff-highlight
    fi
    [ -x "$DIFF_FILTER" ] || DIFF_FILTER=cat
  fi
  diff -U 1 -w -d --suppress-common-lines $B $S > $D || true #cat $D | $DIFF_FILTER
fi

eval "$(cat out/sinspect_prev 2>/dev/null)" || true

NBYTE=$(stat -f %z $S)
NINSTR=$(grep -Ev '^\s*(?:[;\.]|\w+\:$)' $S | wc -l | awk '{print $1}')
NBR=$(grep -Ei '^\s*B(?:cc|L|LR|R)|CBN?Z|RET|TBN?Z\s' $S | wc -l | awk '{print $1}')
NLB=$(grep -Ev '^\s*[;\.]' $S | grep -E '^[^_][0-9A-Za-z_]*\:$' | wc -l | awk '{print $1}')

cat <<END > out/sinspect_prev
PREV_NBYTE=$NBYTE
PREV_NINSTR=$NINSTR
PREV_NBR=$NBR
PREV_NLB=$NLB
END

# ANSI colors: (\e[3Nm or \e[9Nm) 1 red, 2 green, 3 yellow, 4 blue, 5 magenta, 6 cyan
_report() {
  local LABEL=$1;shift
  local DELTA=$(( $1 - ${2:-$1} ))
  local ICON="—"
  (( $DELTA > 0 )) && printf "\e[91m" && ICON="▲"
  (( $DELTA < 0 )) && printf "\e[92m" && ICON="▼"
  (( $DELTA == 0 )) && printf "\e[2m"
  printf "%-12s %6d %s\e[0m\n" "$LABEL" $DELTA "$ICON"
}

_report "instructions" $NINSTR $PREV_NINSTR
_report "braches" $NBR $PREV_NBR
_report "labels" $NLB $PREV_NLB
_report "B file size" $NBYTE $PREV_NBYTE
