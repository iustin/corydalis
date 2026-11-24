// Copyright (C) 2013 Iustin Pop
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Writes the stamp file of the static files (see "Static files" in
// docs/devel.md): a hash of the names and contents of the files in
// static/ that the server serves, which Settings/StaticFiles.hs
// depends on, so that it is recompiled, embedding the new contents
// (with their new hashes in the URLs of the files), whenever one of
// them changes. Run from the root of the repository, by `npm run build`
// (after the bundles are written) or on its own with `npm run stamp`.

import { createHash } from 'node:crypto';
import { readdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';

/** The stamp file, outside static/, so that it isn't served. */
export const stampFile = '.static-files.stamp';

/**
 * Whether Settings/StaticFiles.hs skips a file or directory, at any
 * level: hidden ones.
 */
function skipped(name) {
  return name.startsWith('.');
}

/** The files below `dir` that the server serves, in a deterministic order. */
function listFiles(dir) {
  const files = [];
  for (const name of readdirSync(dir).sort()) {
    if (skipped(name)) {
      continue;
    }
    const path = `${dir}/${name}`;
    // statSync follows symbolic links, as Settings/StaticFiles.hs does
    // (but a production build fails on broken ones); esbuild, which
    // writes static/, doesn't create any anyway.
    const stats = statSync(path, { throwIfNoEntry: false });
    if (stats === undefined) {
      continue;
    }
    if (stats.isDirectory()) {
      files.push(...listFiles(path));
    } else {
      files.push(path);
    }
  }
  return files;
}

/** The stamp of the files in `dir`: a hash of their names and contents. */
export function staticStamp(dir = 'static') {
  const hash = createHash('sha256');
  for (const path of listFiles(dir)) {
    const contents = createHash('sha256').update(readFileSync(path));
    hash.update(`${path}\0${contents.digest('hex')}\n`);
  }
  return hash.digest('hex');
}

/**
 * Writes the stamp file, only if its contents change, so that tools
 * looking at its modification time don't rebuild needlessly.
 */
export function writeStaticStamp() {
  const stamp = `${staticStamp()}\n`;
  let old = null;
  try {
    old = readFileSync(stampFile, 'utf8');
  } catch (err) {
    if (err.code !== 'ENOENT') {
      throw err;
    }
  }
  if (old !== stamp) {
    writeFileSync(stampFile, stamp);
  }
}

// When run as a script, not imported.
if (import.meta.main) {
  writeStaticStamp();
}
