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

// Records the state of every page with tables (rows, groups, filter
// controls) to OUTDIR/report.json, with screenshots; useful to compare
// before and after a change. See README.md.
//
// Usage: node e2e/pages.cjs OUTDIR

const { chromium } = require('playwright');
const fs = require('fs');
const path = require('path');
const { BASE, collectErrors, login, outDir } = require('./lib.cjs');

const out = outDir();

const PAGES = [
  '/stats/cameras',
  '/stats/lenses',
  '/search/folders/list',
  '/search/images/list',
  '/folder/2021-01-01%20Zurich%20trip%200',
  '/list/countries',
  '/list/people',
  '/curate',
  '/curate/flagged-images',
  '/status/errors',
];

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  const errors = collectErrors(page);
  await login(page);

  const report = {};
  for (const url of PAGES) {
    const resp = await page.goto(BASE + url);
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(300);
    const tables = await page.$$eval('table', (ts) =>
      ts.map((t) => {
        const body = t.tBodies[0];
        const rows = body ? [...body.rows] : [];
        const isGroup = (r) => /group-header/.test(r.className);
        const visible = (r) => r.offsetParent !== null && !/filtered/.test(r.className);
        return {
          id: t.id,
          classes: t.className,
          headers: [...(t.tHead?.rows[0]?.cells ?? [])].map((c) => c.innerText.trim()),
          ariaSort: [...(t.tHead?.rows[0]?.cells ?? [])].map((c) => c.getAttribute('aria-sort')),
          filterControls: t.querySelectorAll('thead input, thead select').length,
          rows: rows.filter((r) => !isGroup(r) && visible(r)).length,
          groups: rows.filter((r) => isGroup(r) && visible(r)).map((r) => r.innerText.trim()).slice(0, 4),
          firstRows: rows.filter(visible).slice(0, 3).map((r) => r.innerText.replace(/\s+/g, ' ').trim().slice(0, 90)),
        };
      }),
    );
    report[url] = { status: resp.status(), tables };
    const name = url.replace(/[^a-z0-9]+/gi, '_').replace(/^_|_$/g, '');
    await page.screenshot({ path: path.join(out, `${name}.png`), fullPage: false });
  }
  report.errors = errors;
  fs.writeFileSync(path.join(out, 'report.json'), JSON.stringify(report, null, 1));
  for (const [url, r] of Object.entries(report)) {
    if (url === 'errors') continue;
    console.log(`${url} -> ${r.status}`);
    for (const t of r.tables) {
      console.log(`  #${t.id} [${t.classes}] rows=${t.rows} filters=${t.filterControls} groups=${JSON.stringify(t.groups)}`);
      console.log(`    first: ${JSON.stringify(t.firstRows[0] ?? '')}`);
    }
  }
  console.log('errors:', errors.length ? errors : 'none');
  await browser.close();
})();
