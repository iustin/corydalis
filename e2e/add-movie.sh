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

# Adds a folder with a short movie to the sample library made by
# setup.sh, for tests/movie.spec.ts (see README.md). Like
# add-problem.sh, use a separate work directory for this. Needs ffmpeg.
#
# The movie is VP9 in an MP4 container: an extension Corydalis knows as
# a movie, and a codec that Playwright's Chromium can play (it has no
# H.264).
#
# Usage: e2e/add-movie.sh WORKDIR

set -euo pipefail

if [ $# -ne 1 ]; then
  echo "Usage: $0 WORKDIR" >&2
  exit 1
fi
LIB=$(realpath -m "$1")/lib
DIR="$LIB/src/2024/2024-12-02 Movie 98"
if [ ! -d "$LIB/src" ]; then
  echo "$LIB/src not found; run e2e/setup.sh with this WORKDIR first" >&2
  exit 1
fi
if [ -e "$DIR" ]; then
  echo "$DIR already exists, refusing to overwrite it" >&2
  exit 1
fi
if ! command -v ffmpeg > /dev/null; then
  echo "ffmpeg not found; it is needed for the movie (see e2e/README.md)" >&2
  exit 1
fi

mkdir -p "$DIR"
# Don't leave an empty or partial folder behind on failure.
trap 'rm -rf "$DIR"' ERR
# Ten seconds of a moving test pattern (long enough not to end while
# tests/movie.spec.ts plays it), small and quick to make.
ffmpeg -nostdin -loglevel error \
  -f lavfi -i testsrc=duration=10:size=320x240:rate=10 \
  -c:v libvpx-vp9 -pix_fmt yuv420p -b:v 100k "$DIR/MOV_0001.mp4"
trap - ERR
echo "Added $DIR, with the movie MOV_0001.mp4"
