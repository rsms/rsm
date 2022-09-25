#!/bin/sh
set -e
cd "$(dirname "$0")"

ARGV0="$0"
BUILD_DIR="$PWD/_build"
DOWNLOAD_DIR="$BUILD_DIR/download"

_err() { echo "$ARGV0:" "$@" >&2 ; exit 1 ; }

# _checksum [-sha256|-sha512] [<file>]
# Prints the sha1 (or sha256 or sha512) sum of file's content
# (or stdin if no <file> is given)
_checksum() {
  local prog=sha1sum
  if [ "$1" = "-sha256" ]; then prog=sha256sum; shift; fi
  if [ "$1" = "-sha512" ]; then prog=sha512sum; shift; fi
  $prog "$@" | cut -f 1 -d ' '
}

# _verify_checksum [-silent] file checksum
# checksum can be prefixed with sha1: sha256: or sha512: (e.g. sha256:checksum)
_verify_checksum() {
  local silent
  if [ "$1" = "-silent" ]; then silent=y; shift; fi
  local file="$1"
  local expected="$2"
  local prog=sha1sum
  case "$expected" in
    sha1:*)   expected=${expected:4};;
    sha256:*) expected=${expected:7}; prog=sha256sum ;;
    sha512:*) expected=${expected:7}; prog=sha512sum ;;
  esac
  [ -f "$file" ] || _err "_verify_checksum: $file not found"
  command -v "$prog" >/dev/null || _err "_verify_checksum: $prog not found in PATH"
  local actual=$("$prog" "$file" | cut -f 1 -d ' ')
  if [ "$expected" != "$actual" ]; then
    if [ -z "$silent" ]; then
      echo "Checksum mismatch: $file" >&2
      echo "  Actual:   $actual" >&2
      echo "  Expected: $expected" >&2
    fi
    return 1
  fi
}

# _downloaded_file filename|url
# Prints absolute path to a file downloaded by _download
_downloaded_file() {
  echo "$DOWNLOAD_DIR/$(basename "$1")"
}

# _download url checksum [filename]
# Download file from url. If filename is not given (basename url) is used.
# If DOWNLOAD_DIR/filename exists, then only download if the checksum does not match.
_download() {
  local url="$1"
  local checksum="$2"
  local filename="$DOWNLOAD_DIR/$(basename "${3:-"$url"}")"
  echo "filename $filename"
  while [ ! -e "$filename" ] || ! _verify_checksum -silent "$filename" "$checksum"; do
    if [ -n "$did_download" ]; then
      echo "Checksum for $filename failed" >&2
      echo "  Actual:   $(_checksum "$filename")" >&2
      echo "  Expected: $checksum" >&2
      return 1
    fi
    rm -rf "$filename"
    echo "fetch $url"
    mkdir -p "$(dirname "$filename")"
    curl -L --progress-bar -o "$filename" "$url"
    did_download=y
  done
}

# _extract_tar tarfile outdir
_extract_tar() {
  local tarfile="$1"
  local outdir="$2"
  local name=$(basename "$tarfile")
  [ -e "$tarfile" ] || _err "$tarfile not found"
  local extract_dir="$BUILD_DIR/.extract-$name"
  rm -rf "$extract_dir"
  mkdir -p "$extract_dir"
  echo "extracting ${tarfile##$PWD/} -> ${outdir##$PWD/}"
  XZ_OPT='-T0' tar -C "$extract_dir" -xf "$tarfile"
  rm -rf "$outdir"
  mkdir -p "$(dirname "$outdir")"
  mv -f "$extract_dir"/* "$outdir"
  rm -rf "$extract_dir"
}

# _file_is_newer ref_file src_file...
# Returns true (0) if any src_file is newer (more recently modified) than ref_file.
_file_is_newer() {
  local REF_FILE=$1 ; shift
  for f in $@; do
    [[ "$f" -nt "$REF_FILE" ]] && return 0
  done
  return 1
}

# npm info rsms-mkweb
MKWEB_VERSION=0.2.3
MKWEB_URL=https://registry.npmjs.org/rsms-mkweb/-/rsms-mkweb-${MKWEB_VERSION}.tgz
MKWEB_SHA1=701078863a37b08af79117e011d4be8a01873d20
MKWEB_ARCHIVE=mkweb-${MKWEB_VERSION}.tgz
MKWEB_EXE=$BUILD_DIR/mkweb/mkweb-${MKWEB_VERSION}

if ! [ -f "$MKWEB_EXE" ]; then
  _download "$MKWEB_URL" $MKWEB_SHA1 "$MKWEB_ARCHIVE"
  MKWEB_DIR=$(dirname "$MKWEB_EXE")
  rm -rf "$MKWEB_DIR"
  _extract_tar "$(_downloaded_file "$MKWEB_ARCHIVE")" "$MKWEB_DIR"
  (cd "$MKWEB_DIR" &&
    npm i --omit dev --no-audit --no-bin-links --no-fund --no-package-lock)
  cp "$MKWEB_DIR/dist/mkweb" "$MKWEB_EXE"
  chmod +x "$MKWEB_EXE"
fi


# rebuild favicon in case the source files have changed
if _file_is_newer favicon.ico _favicon/*.png ; then
  if which convert >/dev/null; then
    echo "generate favicon.ico from" _favicon/*.png
    convert _favicon/*.png favicon.ico
  else
    echo 'convert not found in $PATH. Skipping favicon.ico.' >&2
    echo '(try `brew install convert`)' >&2
  fi
fi


MKWEB_ARGS=( -opt )
# MKWEB_ARGS=+( -verbose )
# if [ -z "$1" -o "$1" != "-w" ]; then
#   MKWEB_ARGS+=( -opt )
# fi

#MKWEB_EXE=$HOME/src/mkweb/mkweb.js

exec "$MKWEB_EXE" "${MKWEB_ARGS[@]}" "$@"
