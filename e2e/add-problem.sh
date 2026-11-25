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

# Adds a folder with a picture that has a problem to the sample library
# made by setup.sh, for tests/public-site.spec.ts (see README.md). The other
# scripts expect the library as setup.sh makes it, so use a separate
# work directory for this.
#
# Usage: e2e/add-problem.sh WORKDIR

set -euo pipefail

if [ $# -ne 1 ]; then
  echo "Usage: $0 WORKDIR" >&2
  exit 1
fi
LIB=$(realpath -m "$1")/lib
DIR="$LIB/src/2024/2024-12-01 Broken 99"
# Check everything before creating anything, so that a wrong WORKDIR
# leaves nothing behind.
if [ ! -d "$LIB/src" ]; then
  echo "$LIB/src not found; run e2e/setup.sh with this WORKDIR first" >&2
  exit 1
fi
if [ -e "$DIR" ]; then
  echo "$DIR already exists, refusing to overwrite it" >&2
  exit 1
fi
# A good picture, and a truncated copy of it, for which exiftool warns
# "JPEG format error", i.e. the picture's problem (and whose previews
# fail, which shows the preview error image).
good=$(find "$LIB/src" -name 'IMG_0001.jpg' | head -n 1)
if [ -z "$good" ]; then
  echo "IMG_0001.jpg not found under $LIB/src; is it the library made by e2e/setup.sh?" >&2
  exit 1
fi

mkdir -p "$DIR"
cp -p "$good" "$DIR/IMG_GOOD.jpg"
head -c 2000 "$good" > "$DIR/IMG_BAD.jpg"
echo "Added $DIR, with IMG_GOOD and the truncated IMG_BAD"
