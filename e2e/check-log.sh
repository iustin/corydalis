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
# the browser can't see: an error page (5xx) or an error or warning
# message, e.g. when a tool fails and the server sends an error image
# with a 200. Lines matching one of the ALLOWED regexps (grep -E) are
# expected: they are ignored, and each must match at least one line (so
# that a change in what is expected doesn't go unnoticed).
#
# The server's own messages have no level, so warnings and errors are
# found by their words. Messages written after the last request may
# still be buffered, and so not checked.
#
# Usage: e2e/check-log.sh LOG [ALLOWED...]

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 LOG [ALLOWED...]" >&2
  exit 1
fi
log=$1; shift
if [ ! -f "$log" ] || [ ! -r "$log" ]; then
  echo "$log not found or not readable" >&2
  exit 1
fi

# Like grep -a (so that odd bytes don't hide lines), but fails on a real
# error (status 2, e.g. a bad regexp), instead of "no lines".
agrep() {
  local rc=0
  grep -a "$@" || rc=$?
  if [ $rc -gt 1 ]; then
    echo "grep $* failed" >&2
    exit 2
  fi
}

# Access log lines (Apache format) start with the client's address; the
# others are the server's own messages. In access lines, only the
# status matters (a URL can contain "error"); in the others, the words
# that mean trouble.
access='^[0-9a-fA-F.:]+ - [^ ]+ \['
if [ -z "$(agrep -E "$access" "$log")" ]; then
  echo "No requests logged in $log, is it the right log?" >&2
  exit 1
fi
words='error|exception|fail|panic|warn|skipping|ignored|cannot|does not exist|overflow|non-exhaustive|<<loop>>'
bad=$(agrep -E "$access.*\" 5[0-9][0-9] " "$log")
bad+=$'\n'$(agrep -Ev "$access" "$log" | agrep -Ei "$words")
for allowed in "$@"; do
  if [ -z "$allowed" ]; then
    echo "Empty ALLOWED regexp" >&2
    exit 2
  fi
  if [ -z "$(agrep -E -- "$allowed" "$log")" ]; then
    echo "Expected lines matching '$allowed' not found in $log"
    exit 1
  fi
  bad=$(agrep -Ev -- "$allowed" <<<"$bad")
done
bad=$(agrep -v '^$' <<<"$bad")
if [ -n "$bad" ]; then
  echo "Unexpected errors in $log:"
  echo "$bad"
  exit 1
fi
echo "No unexpected errors in $log"
