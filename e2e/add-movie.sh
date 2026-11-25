#!/bin/bash
#
# Copyright (C) 2013 Iustin Pop
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Adds a short movie to the sample library made by setup.sh, for
# tests/movie.spec.ts (see README.md). Needs ffmpeg.
#
# By default, the movie is on its own, in a new folder (2024-12-02
# Movie 98, as MOV_0001), so the server shows its first frame, which it
# extracts with ffmpeg. The other scripts expect the library as
# setup.sh makes it, so use a separate work directory for this, as for
# add-problem.sh.
#
# With --paired, the movie goes next to an existing picture
# (IMG_0005 in 2021-01-01 Zurich trip 0), as some cameras record them,
# and the server shows that picture. This keeps the folder and image
# counts of the library, so the other scripts still work.
#
# The movie is VP9 in an MP4 container: an extension Corydalis knows as
# a movie, and a codec that Playwright's Chromium can play (it has no
# H.264).
#
# Usage: e2e/add-movie.sh [--paired] WORKDIR

set -euo pipefail

paired=false
if [ "${1:-}" = "--paired" ]; then
  paired=true
  shift
fi
if [ $# -ne 1 ]; then
  echo "Usage: $0 [--paired] WORKDIR" >&2
  exit 1
fi
LIB=$(realpath -m "$1")/lib
if [ ! -d "$LIB/src" ]; then
  echo "$LIB/src not found; run e2e/setup.sh with this WORKDIR first" >&2
  exit 1
fi
if $paired; then
  DIR="$LIB/src/2021/2021-01-01 Zurich trip 0"
  NAME=IMG_0005
  if [ ! -f "$DIR/$NAME.jpg" ]; then
    echo "$DIR/$NAME.jpg not found; is it the library made by e2e/setup.sh?" >&2
    exit 1
  fi
else
  DIR="$LIB/src/2024/2024-12-02 Movie 98"
  NAME=MOV_0001
  if [ -e "$DIR" ]; then
    echo "$DIR already exists, refusing to overwrite it" >&2
    exit 1
  fi
fi
OUT="$DIR/$NAME.mp4"
if [ -e "$OUT" ]; then
  echo "$OUT already exists, refusing to overwrite it" >&2
  exit 1
fi
if ! command -v ffmpeg > /dev/null; then
  echo "ffmpeg not found; it is needed for the movie (see e2e/README.md)" >&2
  exit 1
fi

# Don't leave an empty folder or a partial movie behind on failure.
if $paired; then
  trap 'rm -f "$OUT"' ERR
else
  mkdir -p "$DIR"
  trap 'rm -rf "$DIR"' ERR
fi
# Ten seconds of a moving test pattern (long enough not to end while
# tests/movie.spec.ts plays it), small and quick to make.
ffmpeg -nostdin -loglevel error \
  -f lavfi -i testsrc=duration=10:size=320x240:rate=10 \
  -c:v libvpx-vp9 -pix_fmt yuv420p -b:v 100k "$OUT"
trap - ERR
echo "Added the movie $OUT"
