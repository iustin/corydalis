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

# Checks a server log, after an e2e run, for signs of server errors that
# the browser can't see: an error page (5xx) or an error message, e.g.
# when a tool fails and the server sends an error image with a 200.
# Lines matching one of the ALLOWED regexps (grep -E) are expected, and
# ignored.
#
# Usage: e2e/check-log.sh LOG [ALLOWED...]

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 LOG [ALLOWED...]" >&2
  exit 1
fi
log=$1; shift
if [ ! -f "$log" ]; then
  echo "$log not found" >&2
  exit 1
fi

# Access log lines start with the client's address; the others are the
# server's own messages. In access lines, only the status matters (a URL
# can contain "error"); in the others, the words that mean trouble.
access='^[0-9a-fA-F.:]+ - '
bad=$(grep -E "$access.*\" 5[0-9][0-9] " "$log" || true)
bad+=$'\n'$(grep -Ev "$access" "$log" | grep -Ei 'error|exception|fail|panic' || true)
for allowed in "$@"; do
  bad=$(grep -Ev -- "$allowed" <<<"$bad" || true)
done
bad=$(grep -v '^$' <<<"$bad" || true)
if [ -n "$bad" ]; then
  echo "Unexpected errors in $log:"
  echo "$bad"
  exit 1
fi
echo "No unexpected errors in $log"
