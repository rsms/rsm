#!/bin/bash
set -e
cd "$(dirname "$0")"
_err() { echo -e "$0:" "$@" >&2 ; exit 1; }

OUTDIR=out
BUILD_MODE=safe  # debug | safe | fast
WATCH=
RUN=
NINJA_ARGS=()
TESTING_ENABLED=

while [[ $# -gt 0 ]]; do case "$1" in
  -w)      WATCH=1; shift ;;
  -v)      NINJA_ARGS+=(-v); shift ;;
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
    echo -e "\x1bc"  # clear screen ("scroll to top" style)
    BUILD_OK=1
    bash "./$(basename "$0")" "-$BUILD_MODE" "${NINJA_ARGS[@]}" "$@" || BUILD_OK=
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
            --exclude='.*' --include='\.(c|cc|cpp|m|mm|h|hh|sh|py)$' \
            --recursive \
            src $(basename "$0")
  done
  exit 0
fi


# use llvm at from homebrew if available
if [ -x /usr/local/opt/llvm/bin/clang ]; then
  export PATH=/usr/local/opt/llvm/bin:$PATH
  export CC=clang
  export CXX=clang++
  export AR=llvm-ar
  CC_IS_CLANG=1
else
  export CC=${CC:-cc}
fi
CC_PATH=$(command -v $CC)
CC_PATH=${CC_PATH##$PWD/}
[ -f "$CC_PATH" ] || _err "CC (\"$CC\") not found"
if [ -z "$CC_IS_CLANG" ] && $CC --version 2>/dev/null | head -n1 | grep -q clang; then
  CC_IS_CLANG=1
fi

# check compiler and clear $OUTDIR if cflags or compiler changed
CCONFIG_FILE=$OUTDIR/cconfig.txt
CCONFIG="$CC_PATH: $(sha256sum "$CC_PATH" | cut -d' ' -f1)"
if [ "$(cat "$CCONFIG_FILE" 2>/dev/null)" != "$CCONFIG" ]; then
  [ -f "$CCONFIG_FILE" ] && echo "compiler config changed"
  rm -rf "$OUTDIR"
  mkdir -p "$OUTDIR"
  echo "$CCONFIG" > "$CCONFIG_FILE"
fi


CFLAGS=( $CFLAGS )
LDFLAGS=( $LDFLAGS )

CFLAGS+=( -DR_WITH_LIBC )

if [ "$BUILD_MODE" != "debug" ]; then
  CFLAGS+=( -O3 -mtune=native -DNDEBUG )
  [ "$BUILD_MODE" = "safe" ] && CFLAGS+=( -DR_SAFE )
  LDFLAGS+=( -flto )
  # LDFLAGS+=( -dead_strip )
else
  CFLAGS+=( -DDEBUG -ferror-limit=6 )
  [ -n "$TESTING_ENABLED" ] && CFLAGS+=( -DR_TESTING_ENABLED )
fi


# enable llvm address and UD sanitizer in debug builds
if [ -n "$CC_IS_CLANG" -a "$BUILD_MODE" = "debug" ]; then
  # See https://clang.llvm.org/docs/AddressSanitizer.html
  # See https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html
  CFLAGS+=(
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
  )
  LDFLAGS+=(
    -fsanitize=address,undefined \
  )
fi

# Note: -fms-extensions enables composable structs in clang & GCC
# See https://gcc.gnu.org/onlinedocs/gcc-11.2.0/gcc/Unnamed-Fields.html
# TODO: test with gcc; may require -fplan9-extensions

cat << _END > build.ninja
ninja_required_version = 1.3
builddir = $OUTDIR
objdir = \$builddir/$BUILD_MODE

cflags = $
  -g $
  -fcolor-diagnostics $
  -feliminate-unused-debug-types $
  -Wall -Wextra -Wvla $
  -Wimplicit-fallthrough $
  -Wno-missing-field-initializers $
  -Wno-unused-parameter $
  -Werror=implicit-function-declaration $
  -Werror=incompatible-pointer-types $
  -Wcovered-switch-default ${CFLAGS[@]}

cflags_c = $
  -std=c11 -fms-extensions -Wno-microsoft

cflags_cxx = $
  -std=c++14 $
  -fvisibility-inlines-hidden $
  -fno-exceptions $
  -fno-rtti

ldflags = -g ${LDFLAGS[@]}

rule link
  command = $CC \$ldflags -o \$out \$in
  description = link \$out

rule cc
  command = $CC -MMD -MF \$out.d \$cflags \$cflags_c -c \$in -o \$out
  depfile = \$out.d
  description = compile \$in

rule cxx
  command = $CXX -MMD -MF \$out.d \$cflags \$cflags_cxx -c \$in -o \$out
  depfile = \$out.d
  description = compile \$in

_END


_objfile() { echo \$objdir/${1//\//.}.o; }
_gen_obj_build_rules() {
  local CFLAGS=$1 ; shift
  local CXXFLAGS=$1 ; shift
  local OBJECT
  for SOURCE in "$@"; do
    OBJECT=$(_objfile "$SOURCE")
    case "$SOURCE" in
      *.c|*.m)
        echo "build $OBJECT: cc $SOURCE" >> build.ninja
        [ -n "$CFLAGS" ] &&
          echo "  cflags = \$cflags $CFLAGS" >> build.ninja
        ;;
      *.cc|*.cpp|*.mm)
        echo "build $OBJECT: cxx $SOURCE" >> build.ninja
        [ -n "$CXXFLAGS" ] &&
          echo "  cflags = \$cflags $CXXFLAGS" >> build.ninja
        ;;
      *) _err "don't know how to compile this file type ($SOURCE)"
    esac
    echo "$OBJECT"
  done
}

if [ -n "$TESTING_ENABLED" ]; then
  R_SOURCES=( $(find src -name '*.c') )
else
  R_SOURCES=( $(find src -name '*.c' -not -name '*_test.c' -not -name 'test.c') )
fi

R_OBJECTS=( $(_gen_obj_build_rules "" "" "${R_SOURCES[@]}") )
echo >> build.ninja

echo "build rsm: phony \$builddir/rsm" >> build.ninja
echo "build \$builddir/rsm: link ${R_OBJECTS[@]}" >> build.ninja
echo >> build.ninja

echo "default rsm" >> build.ninja

echo ninja "${NINJA_ARGS[@]}" "$@"
if [ -n "$RUN" ]; then
  ninja "${NINJA_ARGS[@]}" "$@"
  echo $RUN
  exec $SHELL -c "$RUN"
fi
exec ninja "${NINJA_ARGS[@]}" "$@"
