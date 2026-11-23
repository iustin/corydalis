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

# Runs all the e2e checks (see README.md) from scratch: builds the
# binaries (normal and public site, unoptimised, in their own stack work
# directories), makes the two sample libraries, starts a server for
# each, runs the scripts against them, and stops the servers. Exits
# with an error if any script failed. Run from the root of the
# repository, after the frontend bundles were built (npm run build).
#
# Usage: e2e/run.sh [WORKDIR]   (default: _run/e2e, recreated)
#
# Environment: STACK (default: stack) and STACK_ARGS, e.g.
# STACK_ARGS="--ghc-build tinfo6" for older stack versions.

set -euo pipefail

if [ ! -f config/settings.yml.sample ]; then
  echo "Run this from the root of the repository" >&2
  exit 1
fi
WORK=$(realpath -m "${1:-_run/e2e}")
STACK=${STACK:-stack}
read -r -a STACK_ARGS <<<"${STACK_ARGS:-}"
export LANG=C.UTF-8 LC_ALL=C.UTF-8

rm -rf "$WORK"
mkdir -p "$WORK"

build() {
  local workdir=$1 bin=$2; shift 2
  # -O0 overrides the -O2 of non-dev builds: much faster to compile,
  # and the behaviour checked here doesn't depend on optimisation.
  "$STACK" "${STACK_ARGS[@]}" --work-dir "$workdir" build \
    --flag corydalis:-dev "$@" --ghc-options=-O0 \
    --copy-bins --local-bin-path "$bin"
}
echo "== Building the binaries"
build .stack-e2e "$WORK/bin"
build .stack-e2e-public "$WORK/public-bin" --flag corydalis:public-site

echo "== Making the libraries"
# Both at once, as they take a while (one exiftool run per image).
e2e/setup.sh "$WORK/normal" "$WORK/bin" &
normal_setup=$!
e2e/setup.sh "$WORK/public" "$WORK/public-bin"
e2e/add-problem.sh "$WORK/public"
wait "$normal_setup"

PIDS=()
cleanup() {
  for p in "${PIDS[@]}"; do kill "$p" 2>/dev/null || true; done
}
trap cleanup EXIT

# Starts a server for the given library on the given port, and waits
# until it answers.
start() {
  local name=$1 bin=$2 port=$3
  PORT=$port APPROOT=http://localhost:$port \
    "$bin/corydalis" "$WORK/$name/settings.yml" > "$WORK/$name/server.log" 2>&1 &
  local pid=$!
  PIDS+=("$pid")
  for _ in $(seq 1 60); do
    # Checked first, so that another server already on the port (whose
    # answers the curl would get) is reported, not used.
    if ! kill -0 "$pid" 2>/dev/null; then break; fi
    if curl -sf -o /dev/null "http://localhost:$port/auth/login"; then return; fi
    sleep 1
  done
  echo "The $name server didn't start; its log:" >&2
  cat "$WORK/$name/server.log" >&2
  exit 1
}
start normal "$WORK/bin" 3000
start public "$WORK/public-bin" 3001

FAILED=()
# Runs one script, recording (but not stopping on) its failure.
check() {
  local name=$1; shift
  echo "== $name"
  local start=$SECONDS
  if "$@"; then
    echo "-- $name: passed in $((SECONDS - start))s"
  else
    echo "-- $name: FAILED in $((SECONDS - start))s"
    FAILED+=("$name")
  fi
}

export CORYDALIS_URL=http://localhost:3000
export CORYDALIS_PASSWORD_FILE=$WORK/normal/password
check smoke node e2e/smoke.cjs "$WORK/out/smoke"
check verify node e2e/verify.cjs "$WORK/out/verify"
# pages.cjs and shots.cjs only record (reports and screenshots, for a
# person to look at); they fail only if they crash or miss an element.
check "pages (records only)" node e2e/pages.cjs "$WORK/out/pages"
# plots.cjs only records; comparing a report with itself fails only if
# the run had console errors.
check plots bash -c 'node e2e/plots.cjs "$0" && node e2e/plots.cjs --compare "$0" "$0"' \
  "$WORK/out/plots"

export CORYDALIS_URL=http://localhost:3001
export CORYDALIS_PASSWORD_FILE=$WORK/public/password
check public-site node e2e/public-site.cjs "$WORK/public"
# shots.cjs needs a library with problems, so it uses the public one.
check "shots (records only)" node e2e/shots.cjs "$WORK/out/shots"

names=(normal public)
for i in "${!names[@]}"; do
  if ! kill -0 "${PIDS[$i]}" 2>/dev/null; then
    echo "The ${names[$i]} server died during the run; see $WORK/${names[$i]}/server.log"
    FAILED+=("${names[$i]} server")
  fi
done

if [ ${#FAILED[@]} -gt 0 ]; then
  echo "Failed: ${FAILED[*]} (output in $WORK/out, server logs in $WORK/*/server.log)"
  exit 1
fi
echo "All e2e checks passed (output in $WORK/out)"
