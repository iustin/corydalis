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

// Smoke test: as a logged in user, loads every page of the application
// and checks that it answers with success, renders its navigation bar,
// and causes no browser console errors. Uses the library made by
// setup.sh; see README.md.
//
// Usage: node e2e/smoke.cjs OUTDIR (for screenshots of failing pages)

const { chromium } = require('playwright');
const path = require('path');
const { BASE, collectErrors, login, outDir } = require('./lib.cjs');

const out = outDir();
const FOLDER = encodeURIComponent('2021-01-01 Zurich trip 0');

// Pages with fixed URLs; the lens and camera pages are found by
// following the first link on their list pages.
const PAGES = [
  '/',
  '/about',
  '/curate',
  '/curate/flagged-images',
  '/search/folders/list',
  '/search/folders/by-year/2021',
  '/search/folders/no-year',
  '/search/images/list',
  '/search/images/grid/0',
  '/search/images/view',
  '/search?country=Japan',
  '/search/quick?q=Zurich',
  `/folder/${FOLDER}`,
  `/folder/${FOLDER}/images/IMG_0001`,
  `/view/${FOLDER}/IMG_0001`,
  '/settings',
  '/status',
  '/status/errors',
  '/stats/lenses',
  '/stats/cameras',
  ...[
    'countries',
    'cities',
    'people',
    'keywords',
    'years',
    'months',
    'cameras',
    'lenses',
    'f-stops',
    'iso',
    'focal-length',
  ].map((s) => `/list/${s}`),
];

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({
    viewport: { width: 1280, height: 900 },
  });
  await login(page);
  let failures = 0;

  async function visit(url) {
    const errors = collectErrors(page);
    const problems = [];
    try {
      const resp = await page.goto(BASE + url, { waitUntil: 'networkidle' });
      if (!resp || !resp.ok()) problems.push(`status ${resp && resp.status()}`);
      // The login page has a navigation bar too, so check that we were
      // not sent there (e.g. after losing the session).
      if (new URL(page.url()).pathname.startsWith('/auth/'))
        problems.push(`redirected to ${page.url()}`);
      if ((await page.locator('nav.navbar').count()) === 0)
        problems.push('no navigation bar');
    } catch (e) {
      problems.push(e.message.split('\n')[0]);
    }
    page.removeAllListeners('console');
    page.removeAllListeners('pageerror');
    problems.push(...errors);
    if (problems.length) {
      failures++;
      console.log(`FAIL ${url}: ${problems.join('; ')}`);
      const name = url.replace(/[^A-Za-z0-9]+/g, '_');
      await page
        .screenshot({ path: path.join(out, `${name}.png`) })
        .catch((e) => console.log(`  (no screenshot: ${e.message})`));
    } else {
      console.log(`ok   ${url}`);
    }
  }

  for (const url of PAGES) await visit(url);
  for (const [list, prefix] of [
    ['/stats/lenses', '/stats/lens/'],
    ['/stats/cameras', '/stats/camera/'],
  ]) {
    await page.goto(BASE + list);
    const href = await page
      .getAttribute(`a[href*="${prefix}"]`, 'href')
      .catch((e) => {
        console.log(`  (${list}: ${e.message.split('\n')[0]})`);
        return null;
      });
    if (href) {
      await visit(new URL(href, BASE).pathname);
    } else {
      failures++;
      console.log(`FAIL ${list}: no link to a ${prefix} page`);
    }
  }

  await browser.close();
  console.log(failures ? `${failures} pages failed` : 'ALL OK');
  process.exitCode = failures ? 1 : 0;
})().catch((e) => {
  console.error(e);
  process.exit(1);
});
