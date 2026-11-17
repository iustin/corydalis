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

# Creates a sample library, settings and a test user for the e2e scripts
# (see README.md). Run from the root of the repository.
#
# Usage: e2e/setup.sh WORKDIR BINDIR

set -euo pipefail

if [ $# -ne 2 ] || [ ! -f config/settings.yml.sample ]; then
  echo "Usage (from the repository root): $0 WORKDIR BINDIR" >&2
  exit 1
fi
WORK=$(realpath -m "$1")
BIN=$(realpath "$2")
LIB=$WORK/lib
if [ -e "$LIB" ]; then
  echo "$LIB already exists, refusing to overwrite it" >&2
  exit 1
fi
mkdir -p "$LIB/src" "$LIB/out" "$LIB/cache" "$LIB/db"

# The library: 20 folders over 2021-2024, each with 6 images, from 3
# cameras and 4 lenses, with country, city, people and keywords. Every
# other image also has a processed version, so that statuses vary.
cameras=("NIKON CORPORATION|NIKON Z 6_2" "FUJIFILM|X-T5" "OLYMPUS CORPORATION|E-M1MarkIII")
lenses=("NIKKOR Z 24-70mm f/4 S|24|4" "XF35mmF1.4 R|35|1.4" \
        "M.12-100mm F4.0|12|4" "XF18-55mmF2.8-4 R LM OIS|18|2.8")
places=("Switzerland|Zurich" "Romania|Bucharest" "Japan|Kyoto" "Norway|Bergen" "Italy|Rome")
people=("Alice" "Bob" "Carol" "Dan")
keywords=("mountains" "beach" "city" "flowers" "family")
colors=(SteelBlue DarkOliveGreen IndianRed Goldenrod SlateGray Teal)

n=0
for f in $(seq 0 19); do
  year=$((2021 + f % 4))
  month=$(printf '%02d' $((1 + (f * 5) % 12)))
  day=$(printf '%02d' $((1 + (f * 7) % 27)))
  IFS='|' read -r country city <<<"${places[$((f % 5))]}"
  folder="$year-$month-$day ${city} trip $f"
  mkdir -p "$LIB/src/$year/$folder" "$LIB/out/$year/$folder"
  for i in $(seq 1 6); do
    n=$((n + 1))
    name=$(printf 'IMG_%04d' "$n")
    IFS='|' read -r make model <<<"${cameras[$(((f + i) % 3))]}"
    IFS='|' read -r lens focal aperture <<<"${lenses[$(((f * 3 + i) % 4))]}"
    src="$LIB/src/$year/$folder/$name.jpg"
    convert -size 320x240 "gradient:${colors[$((n % 6))]}-white" \
      -gravity center -pointsize 28 -annotate 0 "$name" "$src"
    exiftool -q -overwrite_original \
      -Make="$make" -Model="$model" -LensModel="$lens" \
      -FocalLength="$focal" -FNumber="$aperture" -ExposureTime=1/250 \
      -ISO=$((100 * i)) \
      -DateTimeOriginal="$year:$month:$day $(printf '%02d' $((8 + i))):15:00" \
      -XMP-photoshop:Country="$country" -XMP-photoshop:City="$city" \
      -XMP-iptcExt:PersonInImage="${people[$((n % 4))]}" \
      -XMP-dc:Subject="${keywords[$((f % 5))]}" \
      -Rating=$((n % 6)) "$src"
    if (( i % 2 == 0 )); then
      cp -p "$src" "$LIB/out/$year/$folder/$name.jpg"
    fi
  done
done
echo "Generated $n images in 20 folders under $LIB"

# Settings: the sample ones, served over plain HTTP, using the library.
sed -e 's/^https:.*/https: false/' \
    -e 's/^secure-sessions:.*/secure-sessions: false/' \
    -e "s#sourcedirs: .*#sourcedirs: [\"$LIB/src\"]#" \
    -e "s#outputdirs: .*#outputdirs: [\"$LIB/out\"]#" \
    -e "s#cachedir: .*#cachedir: \"$LIB/cache\"#" \
    -e "s#_env:SQLITE_DATABASE:db/corydalis.sqlite3#_env:SQLITE_DATABASE:$LIB/db/corydalis.sqlite3#" \
    config/settings.yml.sample > "$WORK/settings.yml"
echo "Wrote $WORK/settings.yml"

# A test user, with a random password.
head -c 18 /dev/urandom | base64 | tr -d '/+=' > "$WORK/password"
chmod 600 "$WORK/password"
LANG=C.UTF-8 LC_ALL=C.UTF-8 "$BIN/db-util" -c "$WORK/settings.yml" \
  add tester < "$WORK/password" > /dev/null
echo "Added user 'tester', password in $WORK/password"
