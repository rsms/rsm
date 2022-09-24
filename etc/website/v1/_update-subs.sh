#!/bin/sh
set -e
cd "$(dirname "$0")"

TMPFILE=${TMPDIR:-/tmp}/rsm-subs-$$

grep -E 'src="https://www.youtube-nocookie.com/embed/|##' index.md | sed -E 's@^.+\.com/embed/([^\?]+).+@\1@g' > "$TMPFILE"

TITLE=
while IFS= read -r line; do
  case "$line" in
    "##"*) TITLE=$(echo ${line:2}) ;;
    *)
      if [ -n "$TITLE" ]; then
        YTID=$line
        SHORTTITLE=ep$(echo "$TITLE" | sed -E 's/[^0-9]+//g')
        SUBFILE=$SHORTTITLE.vtt
        if ! [ -f "$SUBFILE" ]; then
          youtube-dl --sub-lang en --write-auto-sub --skip-download --id \
            https://www.youtube.com/watch\?v\=$YTID
          mv $YTID.en.vtt $SUBFILE || echo "no subs for $TITLE -- (try again later)"
        fi
        [ "$SUBFILE" -nt "$SHORTTITLE.html" ] &&
          node _gentranscript.js "$SUBFILE" "$TITLE" "$SHORTTITLE.html"
        TITLE= # skip extra/additional videos
      fi
      ;;
  esac
done < "$TMPFILE"

# youtube-dl --sub-lang en --write-auto-sub --skip-download https://www.youtube.com/watch\?v\=A9KtyRzk40Q
