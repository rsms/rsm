#!/bin/bash
set -e
cd "$(dirname "$0")"
_err() { echo -e "$0:" "$@" >&2 ; exit 1; }

OUTDIR=out
BUILD_MODE=safe  # debug | safe | fast
WATCH=
WATCH_ADDL_FILES=()
_WATCHED=
RUN=
NINJA_ARGS=()
TESTING_ENABLED=

while [[ $# -gt 0 ]]; do case "$1" in
  -v)      NINJA_ARGS+=(-v); shift ;;
  -w)      WATCH=1; shift ;;
  -_w_)    _WATCHED=1; shift ;;
  -wf=*)   WATCH=1; WATCH_ADDL_FILES+=( "${1:4}" ); shift ;;
  -run=*)  RUN=${1:5}; shift ;;
  -debug)  BUILD_MODE=debug; TESTING_ENABLED=1; shift ;;
  -safe)   BUILD_MODE=safe; TESTING_ENABLED=; shift ;;
  -fast)   BUILD_MODE=fast; TESTING_ENABLED=; shift ;;
  -h|-help|--help) cat << _END
usage: $0 [options] [<target> ...]
options:
  -safe      Build optimized product with some assertions enabled (default)
  -fast      Build optimized product without any assertions
  -debug     Build debug product
  -w         Rebuild as sources change
  -wf=<file> Watch <file> for changes (can be provided multiple times)
  -run=<cmd> Run <cmd> after successful build
  -help      Show help on stdout and exit
_END
    exit ;;
  --) break ;;
  -*) _err "unknown option: $1" ;;
  *) break ;;
esac; done

# -w to enter "watch & build & run" mode
if [ -n "$WATCH" ]; then
  command -v fswatch >/dev/null || _err "fswatch not found in PATH"
  # case "$RUN" in
  #   *" "*) RUN="$SHELL -c '$RUN'" ;;
  # esac
  RUN_PIDFILE=${TMPDIR:-/tmp}/$(basename "$(dirname "$PWD")").build-runpid.$$
  echo "RUN_PIDFILE=$RUN_PIDFILE"
  _killcmd() {
    local RUN_PID=$(cat "$RUN_PIDFILE" 2>/dev/null)
    if [ -n "$RUN_PID" ]; then
      kill $RUN_PID 2>/dev/null && echo "killing #$RUN_PID"
      ( sleep 0.1 ; kill -9 "$RUN_PID" 2>/dev/null || true ) &
      rm -f "$RUN_PIDFILE"
    fi
  }
  _exit() {
    _killcmd
    kill $(jobs -p) 2>/dev/null || true
    rm -f "$RUN_PIDFILE"
    exit
  }
  trap _exit SIGINT  # make sure we can ctrl-c in the while loop
  while true; do
    printf "\x1bc"  # clear screen ("scroll to top" style)
    BUILD_OK=1
    ${SHELL:-bash} "./$(basename "$0")" -_w_ "-$BUILD_MODE" "${NINJA_ARGS[@]}" "$@" || BUILD_OK=
    printf "\e[2m> watching files for changes...\e[m\n"
    if [ -n "$BUILD_OK" -a -n "$RUN" ]; then
      export ASAN_OPTIONS=detect_stack_use_after_return=1
      export UBSAN_OPTIONS=print_stacktrace=1
      _killcmd
      ( $SHELL -c "$RUN" &
        RUN_PID=$!
        echo $RUN_PID > "$RUN_PIDFILE"
        echo "$RUN (#$RUN_PID) started"
        wait
        # TODO: get exit code from $RUN
        # Some claim wait sets the exit code, but not in my bash.
        # The idea would be to capture exit code from wait:
        #   status=$?
        echo "$RUN (#$RUN_PID) exited"
      ) &
    fi
    fswatch --one-event --extended --latency=0.1 \
            --exclude='\.(a|o)$' \
            --recursive \
            src "$(basename "$0")" "${WATCH_ADDL_FILES[@]}"
  done
  exit 0
fi

CC_IS_CLANG=false
if [ -z "$CC" ]; then
  # use clang from known preferred location, if available
  if [ -x /usr/local/opt/llvm/bin/clang ]; then
    export PATH=/usr/local/opt/llvm/bin:$PATH
    export CC=clang
    CC_IS_CLANG=true
  elif [ -x /opt/homebrew/opt/llvm/bin/clang ]; then
    export PATH=/opt/homebrew/opt/llvm/bin:$PATH
    export CC=clang
    CC_IS_CLANG=true
  else
    export CC=cc
  fi
fi
CC_PATH=$(command -v "$CC" || true)
[ -f "$CC_PATH" ] || _err "CC (\"$CC\") not found"
if ! $CC_IS_CLANG && $CC --version 2>/dev/null | head -n1 | grep -q clang; then
  CC_IS_CLANG=true
fi
[ -z "$_WATCHED" ] && echo "using compiler $CC_PATH"

DEBUG=false; [ "$BUILD_MODE" = "debug" ] && DEBUG=true

# check compiler and clear $OUTDIR if compiler changed
CCONFIG_FILE=$OUTDIR/cconfig.txt
CCONFIG="$CC_PATH: $(sha256sum "$CC_PATH" | cut -d' ' -f1)"
if [ "$(cat "$CCONFIG_FILE" 2>/dev/null)" != "$CCONFIG" ]; then
  [ -f "$CCONFIG_FILE" ] && echo "compiler config changed"
  rm -rf "$OUTDIR"
  mkdir -p "$OUTDIR"
  echo "$CCONFIG" > "$CCONFIG_FILE"
fi

# flags for all targets (in addition to unconditional flags in ninja template)
CFLAGS=( $CFLAGS )

[ "$BUILD_MODE" = "safe" ] && CFLAGS+=( -DRSM_SAFE )
$DEBUG                     && CFLAGS+=( -O0 -DDEBUG -ferror-limit=6 )
! $DEBUG                   && CFLAGS+=( -DNDEBUG )
[ -n "$TESTING_ENABLED" ]  && CFLAGS+=( -DRSM_TESTING_ENABLED )
! $DEBUG && $CC_IS_CLANG   && CFLAGS+=( -flto )

# target-specific flags (in addition to unconditional flags in ninja template)
CFLAGS_WASM=( -DRSM_NO_LIBC )
CFLAGS_HOST=()
LDFLAGS_HOST=( $LDFLAGS )      # LDFLAGS from env
LDFLAGS_WASM=( $LDFLAGS_WASM ) # LDFLAGS_WASM from env (note: liker is wasm-ld, not cc)

if ! $DEBUG; then
  CFLAGS_HOST+=( -O3 -mtune=native -fomit-frame-pointer )
  CFLAGS_WASM+=( -Oz )
  LDFLAGS_WASM+=( --lto-O3 --no-lto-legacy-pass-manager )
  # LDFLAGS_WASM+=( -z stack-size=$[128 * 1024] ) # larger stack, smaller heap
  # LDFLAGS_WASM+=( --compress-relocations --strip-debug )
  # LDFLAGS_HOST+=( -dead_strip )
  $CC_IS_CLANG && LDFLAGS_HOST+=( -flto )
fi

# enable llvm address and UD sanitizer in debug builds
if $DEBUG && $CC_IS_CLANG; then
  # See https://clang.llvm.org/docs/AddressSanitizer.html
  # See https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html
  CFLAGS_HOST+=(
    -fsanitize=address,undefined \
    \
    -fsanitize-address-use-after-scope \
    \
    -fsanitize=float-divide-by-zero \
    -fsanitize=null \
    -fsanitize=nonnull-attribute \
    -fsanitize=nullability \
    \
    -fno-omit-frame-pointer \
    -fno-optimize-sibling-calls \
    \
    -fmacro-backtrace-limit=0 \
  )
  LDFLAGS_HOST+=(
    -fsanitize=address,undefined \
  )
fi

cat << _END > build.ninja
ninja_required_version = 1.3
builddir = $OUTDIR
objdir = \$builddir/$BUILD_MODE

cflags = $
  -std=c11 $
  -g $
  -fcolor-diagnostics $
  -feliminate-unused-debug-types $
  -fvisibility=hidden $
  -Wall -Wextra -Wvla $
  -Wimplicit-fallthrough $
  -Wno-missing-field-initializers $
  -Wno-unused-parameter $
  -Werror=implicit-function-declaration $
  -Werror=incompatible-pointer-types $
  -Werror=format-insufficient-args $
  -Wcovered-switch-default ${CFLAGS[@]}

cflags_host = ${CFLAGS_HOST[@]}

cflags_wasm = $
  --target=wasm32 $
  --no-standard-libraries $
  -fvisibility=hidden ${CFLAGS_WASM[@]}

ldflags_host = ${LDFLAGS_HOST[@]}

ldflags_wasm = $
  -allow-undefined-file etc/wasm.syms $
  --no-entry $
  --no-gc-sections $
  --export-dynamic $
  --import-memory ${LDFLAGS_WASM[@]}

rule link
  command = $CC \$ldflags_host -o \$out \$in
  description = link \$out

rule link_wasm
  command = wasm-ld \$ldflags_wasm \$in -o \$out
  description = link \$out

rule cc
  command = $CC -MMD -MF \$out.d \$cflags \$cflags_host -c \$in -o \$out
  depfile = \$out.d
  description = compile \$in

rule cc_wasm
  command = $CC -MMD -MF \$out.d \$cflags \$cflags_wasm -c \$in -o \$out
  depfile = \$out.d
  description = compile \$in

_END


_objfile() { echo \$objdir/${1//\//.}.o; }
_gen_obj_build_rules() {
  local FLAVOR=$1 ; shift
  local OBJECT
  local CC_RULE=cc
  [ "$FLAVOR" = wasm ] && CC_RULE=cc_wasm
  for SOURCE in "$@"; do
    OBJECT=$(_objfile "$FLAVOR-$SOURCE")
    case "$SOURCE" in
      *.c|*.m)
        echo "build $OBJECT: $CC_RULE $SOURCE" >> build.ninja
        ;;
      *) _err "don't know how to compile this file type ($SOURCE)"
    esac
    echo "$OBJECT"
  done
}

if [ -n "$TESTING_ENABLED" ]; then
  SOURCES=( $(find src -name '*.c') )
else
  SOURCES=( $(find src -name '*.c' -not -name '*_test.c' -not -name 'test.c') )
fi

HOST_OBJECTS=( $(_gen_obj_build_rules "host" "${SOURCES[@]}") )
WASM_OBJECTS=( $(_gen_obj_build_rules "wasm" "${SOURCES[@]}") )
echo >> build.ninja

echo "build rsm: phony \$builddir/rsm" >> build.ninja
echo "build \$builddir/rsm: link ${HOST_OBJECTS[@]}" >> build.ninja
echo >> build.ninja

echo "build rsm.wasm: phony \$builddir/rsm.wasm" >> build.ninja
echo "build \$builddir/rsm.wasm: link_wasm ${WASM_OBJECTS[@]}" >> build.ninja
echo >> build.ninja

echo "default rsm" >> build.ninja

if [ -n "$RUN" ]; then
  ninja "${NINJA_ARGS[@]}" "$@"
  echo $RUN
  exec $SHELL -c "$RUN"
fi
exec ninja "${NINJA_ARGS[@]}" "$@"
