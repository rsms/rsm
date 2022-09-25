#!/bin/bash
set -e
cd "$(dirname "$0")"
_err() { echo -e "$0:" "$@" >&2 ; exit 1; }

OUTDIR=
OUTDIR_DEFAULT=out
BUILD_MODE=safe  # debug | safe | fast
WATCH=
WATCH_ADDL_FILES=()
_WATCHED=
RUN=
NINJA_ARGS=()
NON_WATCH_ARGS=()
EXTRA_CFLAGS=()
TESTING_ENABLED=
STRIP=false
STATIC=false
DEBUGGABLE=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    -w)      WATCH=1; shift; continue ;;
    -_w_)    _WATCHED=1; shift; continue ;;
    -wf=*)   WATCH=1; WATCH_ADDL_FILES+=( "${1:4}" ); shift; continue ;;
    -run=*)  RUN=${1:5}; shift; continue ;;
  esac
  NON_WATCH_ARGS+=( "$1" )
  case "$1" in
  -safe)   BUILD_MODE=safe; TESTING_ENABLED=; shift ;;
  -fast)   BUILD_MODE=fast; TESTING_ENABLED=; shift ;;
  -debug)  BUILD_MODE=debug; TESTING_ENABLED=1; DEBUGGABLE=true; shift ;;
  -strip)  STRIP=true; DEBUGGABLE=false; shift ;;
  -static) STATIC=true; shift ;;
  -out=*)  OUTDIR=${1:5}; shift; continue ;;
  -g)      DEBUGGABLE=true; shift ;;
  -v)      NINJA_ARGS+=(-v); shift ;;
  -D*)     [ ${#1} -gt 2 ] || _err "Missing NAME after -D";EXTRA_CFLAGS+=( "$1" ); shift ;;
  -h|-help|--help) cat << _END
usage: $0 [options] [--] [<target> ...]
options:
  -safe          Build optimized product with some assertions enabled (default)
  -fast          Build optimized product without any assertions
  -debug         Build debug product
  -strip         Do not include debug data
  -g             Make the build debuggable (debug symbols + basic opt only)
  -w             Rebuild as sources change
  -wf=<file>     Watch <file> for changes (can be provided multiple times)
  -run=<cmd>     Run <cmd> after successful build
  -out=<dir>     Build in <dir> instead of "$OUTDIR_DEFAULT/<mode>".
  -DNAME[=value] Define CPP variable NAME with value
  -help          Show help on stdout and exit
_END
    exit ;;
  --) break ;;
  -*) _err "unknown option: $1" ;;
  *) break ;;
esac; done


# -w to enter "watch & build & run" mode
if [ -n "$WATCH" ]; then
  WATCH_TOOL=fswatch
  # note: inotifywait is part of inotify-tools
  command -v $WATCH_TOOL >/dev/null || WATCH_TOOL=inotifywait
  command -v $WATCH_TOOL >/dev/null ||
    _err "no watch tool available (looked for fswatch and inotifywait in PATH)"

  _fswatch() {
    if [ "$WATCH_TOOL" = fswatch ]; then
      fswatch --one-event --extended --latency=0.1 \
        --exclude='\.(a|o)$' --recursive "$@"
    else
      inotifywait -e modify -e create -e delete -e move -qq \
        --exclude='\.(a|o)$' --recursive "$@"
    fi
  }

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
    bash "./$(basename "$0")" -_w_ "${NON_WATCH_ARGS[@]}" "$@" || BUILD_OK=
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
    _fswatch src "$(basename "$0")" "${WATCH_ADDL_FILES[@]}"
  done
  exit 0
fi

CC_IS_CLANG=false
CC_IS_GCC=false
if [ -n "$CC" ]; then
  CC_BASENAME=$(basename "$CC")
  case "$CC_BASENAME" in
    *" "*)
      CC_FILE=$(dirname "$CC")/$(echo "$CC_BASENAME" | cut -d' ' -f1)
      [ -f "$CC_FILE" ] || CC_FILE=$(command -v "$CC_FILE" || echo "$CC_FILE")
      ;;
    *)
      CC_FILE=$(command -v "$CC" || echo "$CC")
      ;;
  esac
else
  # use clang from known preferred location, if available
  if [ -x /usr/local/opt/llvm/bin/clang ]; then
    export PATH=/usr/local/opt/llvm/bin:$PATH
    export CC=clang
    CC_IS_CLANG=true
  elif [ -x /opt/homebrew/opt/llvm/bin/clang ]; then
    export PATH=/opt/homebrew/opt/llvm/bin:$PATH
    export CC=clang
    CC_IS_CLANG=true
  elif command -v gcc > /dev/null; then
    export CC=gcc
    CC_IS_GCC=true
  else
    export CC=cc
  fi
  CC_FILE=$(command -v "$CC" || true)
  [ -f "$CC_FILE" ] || _err "no suitable compiler found (CC=\"$CC\")"
fi
if ! $CC_IS_CLANG && ! $CC_IS_GCC; then
  case "$($CC --version 2>/dev/null)" in
    *clang*) CC_IS_CLANG=true ;;
    *"Free Software Foundation"*) CC_IS_GCC=true ;;
  esac
fi
[ -z "$_WATCHED" ] && echo "using compiler $CC ($CC_FILE)"

DEBUG=false; [ "$BUILD_MODE" = "debug" ] && DEBUG=true

# set OUTDIR unless set with -out=<dir>
[ -z "$OUTDIR" ] && OUTDIR=$OUTDIR_DEFAULT/$BUILD_MODE

# check compiler and clear $OUTDIR if compiler changed
CCONFIG_FILE=$OUTDIR/cconfig.txt
CCONFIG="$CC_FILE: $(sha256sum "$CC_FILE" | cut -d' ' -f1)"
if [ "$(cat "$CCONFIG_FILE" 2>/dev/null)" != "$CCONFIG" ]; then
  [ -f "$CCONFIG_FILE" ] && echo "compiler config changed"
  rm -rf "$OUTDIR"
  mkdir -p "$OUTDIR"
  echo "$CCONFIG" > "$CCONFIG_FILE"
fi

# flags for all targets (in addition to unconditional flags in ninja template)
CFLAGS=( $CFLAGS $EXTRA_CFLAGS )  # from env
[ "$BUILD_MODE" = "safe" ] && CFLAGS+=( -DRSM_SAFE )
[ -n "$TESTING_ENABLED" ]  && CFLAGS+=( -DRSM_TESTING_ENABLED )

# target-specific flags (in addition to unconditional flags in ninja template)
CFLAGS_WASM=( -DRSM_NO_LIBC )
CFLAGS_HOST=( $CFLAGS_HOST )            # from env
LDFLAGS_HOST=( $LDFLAGS $LDFLAGS_HOST ) # from env
LDFLAGS_WASM=( $LDFLAGS_WASM )          # from env (note: liker is wasm-ld, not cc)

if $DEBUG; then
  CFLAGS+=( -O0 -DDEBUG )
else
  CFLAGS+=( -DNDEBUG )
  # LDFLAGS_WASM+=( -z stack-size=$[128 * 1024] ) # larger stack, smaller heap
  CFLAGS_HOST+=( -O2 )
  CFLAGS_WASM+=( -Oz )
  if ! $DEBUGGABLE; then
    CFLAGS_HOST+=( -fomit-frame-pointer )
    LDFLAGS_HOST+=( -dead_strip )
    LDFLAGS_WASM+=( -O2 --lto-O3 --no-lto-legacy-pass-manager )
    # Link Time Optimization
    if $CC_IS_CLANG; then
      ENABLE_LTO=true
      # LTO can't be used in certain cases when cross compiling
      if [[ "${LDFLAGS_HOST[@]}" == *"-target "* ]]; then
        ENABLE_LTO=false
        # LTO whitelist
        case "${LDFLAGS_HOST[@]}" in
            *"-target x86_64-linux-musl"* \
          | *"-target aarch64-linux-musl"* \
          | *"-target i386-linux-musl"* \
          ) ENABLE_LTO=true ;;
        esac
      fi
      echo "ENABLE_LTO=$ENABLE_LTO"
      if $ENABLE_LTO; then
        CFLAGS+=( -flto )
        LDFLAGS_HOST+=( -flto )
      fi
    fi
  fi
fi

if $STRIP; then
  LDFLAGS_WASM+=( --compress-relocations --strip-debug )
  LDFLAGS_HOST+=( -Wl,-s )
elif $DEBUGGABLE; then
  CFLAGS+=( -g -feliminate-unused-debug-types )
fi

if $STATIC; then
  LDFLAGS_HOST+=( -static )
fi

if $CC_IS_CLANG; then
  CFLAGS+=(
    -fcolor-diagnostics \
    -Wcovered-switch-default \
    -Werror=implicit-function-declaration \
    -Werror=incompatible-pointer-types \
    -Werror=format-insufficient-args \
  )
elif $CC_IS_GCC; then
  CFLAGS+=(
    -fdiagnostics-color \
    -Wswitch-enum \
    -Wno-format-zero-length \
    -Wno-comment \
    -Wno-expansion-to-defined \
    -Wno-type-limits \
  )
fi

# enable llvm address and UD sanitizer in debug builds
if $DEBUG && $CC_IS_CLANG; then
  # See https://clang.llvm.org/docs/AddressSanitizer.html
  # See https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html
  CFLAGS_HOST+=(
    -ferror-limit=6 \
    \
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

NINJAFILE=$OUTDIR/build.ninja

mkdir -p "$(dirname "$OUTDIR")"
cat << _END > "$NINJAFILE"
ninja_required_version = 1.3
builddir = $OUTDIR
objdir = \$builddir/obj

cflags = $
  -std=c11 $
  -fvisibility=hidden $
  -Wall -Wextra -Wvla $
  -Wimplicit-fallthrough $
  -Wno-missing-field-initializers $
  -Wno-unused-parameter $
  ${CFLAGS[@]}

cflags_host = ${CFLAGS_HOST[@]}

cflags_wasm = $
  --target=wasm32 $
  --no-standard-libraries $
  -fvisibility=hidden ${CFLAGS_WASM[@]}

ldflags_host = ${LDFLAGS_HOST[@]}

ldflags_wasm = $
  -allow-undefined-file etc/wasm.syms $
  --no-entry $
  --gc-sections $
  --export-dynamic $
  --import-memory ${LDFLAGS_WASM[@]}

rule link
  command = $CC \$ldflags_host -o \$out \$in
  description = link \$out

rule link_wasm
  command = wasm-ld \$ldflags_wasm \$in -o \$out
  description = link \$out

rule cc
  command = $CC -MMD -MF \$out.d \$cflags \$cflags_host \$flags -c \$in -o \$out
  depfile = \$out.d
  description = compile \$in

rule cc_wasm
  command = $CC -MMD -MF \$out.d \$cflags \$cflags_wasm \$flags -c \$in -o \$out
  depfile = \$out.d
  description = compile \$in

_END


_objfile() { echo \$objdir/${1//\//.}.o; }
_gen_obj_build_rules() {
  local FLAVOR=$1 ; shift
  local CC_EXTRAS=$1 ; shift
  local OBJECT
  local CC_RULE=cc
  [ "$FLAVOR" = "wasm" -o "$FLAVOR" = "wasm-rt" ] && CC_RULE=cc_wasm
  [ -n "$CC_EXTRAS" ] && CC_EXTRAS="flags = $CC_EXTRAS"
  for SOURCE in "$@"; do
    OBJECT=$(_objfile "$FLAVOR-$SOURCE")
    case "$SOURCE" in
      *.c|*.m)
        echo "build $OBJECT: $CC_RULE $SOURCE" >> "$NINJAFILE"
        [ -n "$CC_EXTRAS" ] &&
        echo "  $CC_EXTRAS" >> "$NINJAFILE"
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

HOST_OBJECTS=( $(_gen_obj_build_rules "host" "" "${SOURCES[@]}") )
WASM_OBJECTS=( $(_gen_obj_build_rules "wasm" "" "${SOURCES[@]}") )
WASMRT_OBJECTS=( $(_gen_obj_build_rules "wasm-rt" "-DRSM_NO_ASM=1" "${SOURCES[@]}") )
echo >> "$NINJAFILE"

echo "build rsm: phony \$builddir/rsm" >> "$NINJAFILE"
echo "build \$builddir/rsm: link ${HOST_OBJECTS[@]}" >> "$NINJAFILE"
echo >> "$NINJAFILE"

echo "build rsm.wasm: phony \$builddir/rsm.wasm" >> "$NINJAFILE"
echo "build \$builddir/rsm.wasm: link_wasm ${WASM_OBJECTS[@]}" >> "$NINJAFILE"
echo >> "$NINJAFILE"

echo "build rsm-rt.wasm: phony \$builddir/rsm-rt.wasm" >> "$NINJAFILE"
echo "build \$builddir/rsm-rt.wasm: link_wasm ${WASMRT_OBJECTS[@]}" >> "$NINJAFILE"
echo >> "$NINJAFILE"

echo "default rsm" >> "$NINJAFILE"

if [ -n "$RUN" ]; then
  ninja -f "$NINJAFILE" "${NINJA_ARGS[@]}" "$@"
  echo $RUN
  exec $SHELL -c "$RUN"
fi
exec ninja -f "$NINJAFILE" "${NINJA_ARGS[@]}" "$@"
