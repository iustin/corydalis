/* Copyright (C) 2013 Iustin Pop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

// Shared helpers of the e2e scratchpad scripts; see README.md.

const fs = require('fs');

const BASE = process.env.CORYDALIS_URL || 'http://localhost:3000';
const USER = process.env.CORYDALIS_USER || 'tester';

function password() {
  if (process.env.CORYDALIS_PASSWORD) return process.env.CORYDALIS_PASSWORD;
  const file = process.env.CORYDALIS_PASSWORD_FILE;
  if (!file) {
    throw new Error('Set CORYDALIS_PASSWORD_FILE (or CORYDALIS_PASSWORD)');
  }
  return fs.readFileSync(file, 'utf8').trim();
}

/** Returns the output directory from the command line, creating it. */
function outDir() {
  const out = process.argv[2];
  if (!out) {
    console.error(`Usage: node ${process.argv[1]} OUTDIR`);
    process.exit(2);
  }
  fs.mkdirSync(out, { recursive: true });
  return out;
}

/** Logs in, then waits (up to two minutes) for the scan to find folders. */
async function login(page) {
  await page.goto(BASE + '/auth/login');
  await page.fill('#floatingUsername', USER);
  await page.fill('#floatingPassword', password());
  await Promise.all([
    page.waitForNavigation(),
    page.click('form[action$="/login"] [type=submit]'),
  ]);
  for (let i = 0; i < 60; i++) {
    await page.goto(BASE + '/search/folders/list');
    if ((await page.$$('#folderlist tbody tr')).length > 0) return;
    await page.waitForTimeout(2000);
  }
  throw new Error('No folders found after two minutes; is the scan done?');
}

/** Records page errors and console errors/warnings into an array. */
function collectErrors(page) {
  const errors = [];
  page.on('pageerror', (e) => errors.push(`[${page.url()}] ${e.message}`));
  page.on('console', (m) => {
    if (['error', 'warning'].includes(m.type())) {
      errors.push(`[${page.url()}] ${m.type()}: ${m.text()}`);
    }
  });
  return errors;
}

module.exports = { BASE, collectErrors, login, outDir };
