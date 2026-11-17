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

// Exercises the sortable tables, flagging, the plot pages and the viewer
// on a running Corydalis serving the library from setup.sh (many checks
// depend on it); see README.md.
//
// Usage: node e2e/verify.cjs OUTDIR (for screenshots)

const { chromium } = require('playwright');
const path = require('path');
const assert = require('assert/strict');
const { BASE, collectErrors, login, outDir } = require('./lib.cjs');

const out = outDir();
let failures = 0;

async function check(name, fn) {
  try {
    await fn();
    console.log(`ok   ${name}`);
  } catch (e) {
    failures++;
    console.log(`FAIL ${name}: ${e.message.split('\n')[0]}`);
  }
}

// Visible data rows (not group headers), as arrays of cell texts.
const dataRows = (page, sel) =>
  page.$$eval(`${sel} tbody tr:not(.group-header)`, (rs) =>
    rs.map((r) => [...r.cells].map((c) => c.innerText.trim())),
  );
const groups = (page, sel) =>
  page.$$eval(`${sel} tbody tr.group-header`, (rs) => rs.map((r) => r.innerText.trim()));
const ariaSort = (page, sel) =>
  page.$$eval(`${sel} thead tr:first-child th`, (ths) => ths.map((t) => t.getAttribute('aria-sort')));
const header = (page, sel, n) => page.locator(`${sel} thead tr:first-child th:nth-child(${n}) .sort-button`);
const sortedBy = (rows, col, cmp) => rows.every((r, i) => i === 0 || cmp(rows[i - 1][col], r[col]) <= 0);
const coll = new Intl.Collator(undefined, { numeric: true, sensitivity: 'base' });
const txt = (a, b) => coll.compare(a, b);
const num = (a, b) => parseFloat(a) - parseFloat(b);
const settle = (page) => page.waitForTimeout(250);

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  const errors = collectErrors(page);
  await login(page);

  // --- Folder list: grouping by month, default sort, filters.
  const F = '#folderlist';
  await page.goto(BASE + '/search/folders/list');
  await settle(page);
  await page.screenshot({ path: path.join(out, 'folders.png') });
  await check('folders: all 20 rows, grouped by month of first picture', async () => {
    const rows = await dataRows(page, F);
    assert.equal(rows.length, 20);
    assert.ok(sortedBy(rows, 2, txt), 'sorted by first picture');
    const g = await groups(page, F);
    assert.equal(g[0], 'Jan 2021 (2)');
    const total = g.reduce((s, x) => s + Number(/\((\d+)\)$/.exec(x)[1]), 0);
    assert.equal(total, 20, 'group counts add up');
  });
  await check('folders: aria-sort reflects the initial sort list', async () => {
    const a = await ariaSort(page, F);
    assert.deepEqual(a, [null, 'ascending', 'ascending', 'none', 'none', 'none', 'none']);
  });
  await check('folders: tfoot totals row untouched', async () => {
    const t = await page.$$eval(`${F} tfoot tr`, (rs) => rs.map((r) => r.innerText.replace(/\s+/g, ' ').trim()));
    assert.equal(t.length, 1);
    assert.match(t[0], /^Totals 120/);
  });
  await check('folders: click Images sorts numerically, no groups', async () => {
    await header(page, F, 4).click();
    const rows = await dataRows(page, F);
    assert.ok(sortedBy(rows, 3, num));
    assert.deepEqual(await groups(page, F), []);
    await header(page, F, 4).click();
    assert.equal((await ariaSort(page, F))[3], 'descending');
  });
  await check('folders: click Countries groups by country', async () => {
    await header(page, F, 5).click();
    const g = await groups(page, F);
    assert.deepEqual(g, ['Italy (4)', 'Japan (4)', 'Norway (4)', 'Romania (4)', 'Switzerland (4)']);
  });
  await check('folders: shift-click adds a secondary sort', async () => {
    await header(page, F, 2).click({ modifiers: ['Shift'] });
    const a = await ariaSort(page, F);
    assert.equal(a[4], 'ascending');
    assert.equal(a[1], 'ascending');
    const rows = await dataRows(page, F);
    const italy = rows.filter((r) => r[4] === 'Italy').map((r) => r[1]);
    assert.deepEqual(italy, [...italy].sort(txt));
  });
  await check('folders: text filter (case-insensitive substring)', async () => {
    await page.fill(`${F} .filter-row td:nth-child(6) input`, 'KYO');
    await settle(page);
    const rows = await dataRows(page, F);
    assert.equal(rows.length, 4);
    assert.ok(rows.every((r) => r[5] === 'Kyoto'));
    assert.deepEqual(await groups(page, F), ['Japan (4)']);
    await page.fill(`${F} .filter-row td:nth-child(6) input`, '');
    await settle(page);
    assert.equal((await dataRows(page, F)).length, 20);
  });
  await check('folders: icon column has no filter', async () => {
    assert.equal(await page.locator(`${F} .filter-row td:nth-child(1) *`).count(), 0);
  });
  await check('folders: keyboard sorting via Tab + Enter', async () => {
    await header(page, F, 4).focus();
    await page.keyboard.press('Enter');
    assert.equal((await ariaSort(page, F))[3], 'ascending');
    await page.keyboard.press('Space');
    assert.equal((await ariaSort(page, F))[3], 'descending');
  });

  // --- Image list: select filter on status, data-text values.
  const I = '#imagelist';
  await page.goto(BASE + '/search/images/list');
  await settle(page);
  await page.screenshot({ path: path.join(out, 'images.png') });
  await check('images: 120 rows grouped by folder', async () => {
    assert.equal((await dataRows(page, I)).length, 120);
    const g = await groups(page, I);
    assert.equal(g.length, 20);
    assert.equal(g[0], '2021-01-01 Zurich trip 0 (6)');
  });
  await check('images: no status column, so no select filter', async () => {
    assert.equal(await page.locator(`${I} .filter-row select`).count(), 0);
  });

  await page.goto(BASE + '/folder/2021-01-01%20Zurich%20trip%200');
  await settle(page);
  await page.screenshot({ path: path.join(out, 'folder.png'), fullPage: true });
  await check('folder: status select filter (on data-text)', async () => {
    const sel = page.locator(`${I} .filter-row select`);
    assert.equal(await sel.count(), 1);
    const opts = await sel.locator('option').evaluateAll((os) => os.map((o) => [o.value, o.textContent]));
    console.log('     status options:', JSON.stringify(opts));
    assert.ok(opts.length >= 3, 'at least two statuses plus the empty option');
    const value = opts[1][0];
    await sel.selectOption(value);
    await settle(page);
    const rows = await page.$$eval(`${I} tbody tr:not(.group-header) td:last-child`, (tds) => tds.map((td) => td.dataset.text));
    assert.ok(rows.length > 0 && rows.length < 6, `rows: ${rows.length}`);
    assert.ok(rows.every((t) => t.toLowerCase() === value));
    await sel.selectOption('');
    await settle(page);
    assert.equal((await dataRows(page, I)).length, 6);
  });
  await page.goto(BASE + '/search/images/list');
  await settle(page);

  await check('images: thumbnails column not sortable', async () => {
    assert.equal(await page.locator(`${I} thead tr:first-child th:nth-child(1) .sort-button`).count(), 0);
  });

  // --- Stats pages (plot bundle).
  for (const [url, sel, digitCol] of [
    ['/stats/cameras', '#cameralist', 2],
    ['/stats/lenses', '#lenslist', 4],
  ]) {
    await page.goto(BASE + url);
    await page.waitForLoadState('networkidle');
    await settle(page);
    await page.screenshot({ path: path.join(out, url.replace(/\W+/g, '_') + '.png') });
    await check(`${url}: sorted, sortable, with filters`, async () => {
      const rows = await dataRows(page, sel);
      assert.ok(rows.length >= 3);
      assert.equal((await ariaSort(page, sel))[sel === '#lenslist' ? 4 : 2], 'descending');
      await header(page, sel, digitCol).click();
      assert.ok(sortedBy(await dataRows(page, sel), digitCol - 1, num));
      assert.ok((await page.locator(`${sel} .filter-row input, ${sel} .filter-row select`).count()) > 0);
      assert.deepEqual(await groups(page, sel), []);
    });
  }
  await check('/stats/lenses: type select shows labels, filters on data-text', async () => {
    const sel = page.locator('#lenslist .filter-row select');
    const opts = await sel.locator('option').evaluateAll((os) => os.map((o) => [o.value, o.textContent]));
    assert.ok(opts.some(([v, t]) => v !== t && v !== ''), JSON.stringify(opts));
    console.log('     type options:', JSON.stringify(opts));
    const before = (await dataRows(page, '#lenslist')).length;
    await sel.selectOption(opts[1][0]);
    await settle(page);
    const types = await page.$$eval('#lenslist tbody tr td:nth-child(3)', (tds) => tds.map((td) => td.dataset.text));
    assert.ok(types.length > 0 && types.length <= before, `rows ${types.length} of ${before}`);
    assert.ok(types.every((t) => t === opts[1][0]));
  });

  // --- Numeric operator filters.
  async function filterCount(url, sel, col, value) {
    if (!page.url().endsWith(url)) await page.goto(BASE + url);
    await page.fill(`${sel} .filter-row td:nth-child(${col}) input`, value);
    await settle(page);
    return (await dataRows(page, sel)).length;
  }
  await page.goto(BASE + '/search/folders/list');
  await settle(page);
  await check('operators: placeholder only on numeric columns', async () => {
    const ph = await page.$$eval(`${F} .filter-row input`, (is) => is.map((i) => i.placeholder));
    assert.deepEqual(ph, ['', '', 'e.g. >10', '', '', '']);
  });
  await check('operators: folder Images (all 6)', async () => {
    const u = '/search/folders/list';
    assert.equal(await filterCount(u, F, 4, '>5'), 20);
    assert.equal(await filterCount(u, F, 4, '>6'), 0);
    assert.equal(await filterCount(u, F, 4, '>= 6'), 20);
    assert.equal(await filterCount(u, F, 4, '≤5'), 0);
    assert.equal(await filterCount(u, F, 4, '=6'), 20);
    assert.equal(await filterCount(u, F, 4, '/=6'), 0);
    assert.equal(await filterCount(u, F, 4, '6'), 20, 'bare number is a substring');
    assert.equal(await filterCount(u, F, 4, '7'), 0);
    assert.equal(await filterCount(u, F, 4, '>x'), 0, 'invalid operator is text');
    assert.equal(await filterCount(u, F, 4, ''), 20);
  });
  await check('operators: text columns treat ">" as text', async () => {
    assert.equal(await filterCount('/search/folders/list', F, 5, '>5'), 0);
  });
  await page.goto(BASE + '/stats/cameras');
  await settle(page);
  await check('operators: camera folder count (0) and empty shutter count', async () => {
    const u = '/stats/cameras';
    const C = '#cameralist';
    assert.equal(await filterCount(u, C, 3, '=0'), 3);
    assert.equal(await filterCount(u, C, 3, '>0'), 0);
    await filterCount(u, C, 3, '');
    assert.equal(await filterCount(u, C, 4, '!=0'), 0, 'empty cells never match');
    assert.equal(await filterCount(u, C, 4, ''), 3);
    assert.equal(await filterCount(u, C, 2, '>=40'), 3);
    assert.equal(await filterCount(u, C, 2, '<40'), 0);
  });

  // --- Other table pages.
  for (const [url, sel] of [
    ['/folder/2021-01-01%20Zurich%20trip%200', '#imagelist'],
    ['/list/people', '#itemlist'],
    ['/list/countries', '#itemlist'],
  ]) {
    await page.goto(BASE + url);
    await settle(page);
    await check(`${url}: table enhanced`, async () => {
      assert.ok((await dataRows(page, sel)).length > 0);
      assert.ok((await page.locator(`${sel} .sort-button`).count()) > 0);
      assert.ok((await page.locator(`${sel} .filter-row`).count()) === 1);
    });
  }
  // The status errors table only exists if the scan had errors (e.g.
  // rendering ones, depending on the ImageMagick setup).
  await page.goto(BASE + '/status/errors');
  await settle(page);
  if ((await page.locator('#errorlist').count()) === 0) {
    console.log('skip /status/errors: no errors table (no scan errors)');
  } else {
    await check('/status/errors: table enhanced, grouped by phase', async () => {
      assert.ok((await page.locator('#errorlist .sort-button').count()) > 0);
      assert.match((await groups(page, '#errorlist'))[0], /^\w+ \(\d+\)$/);
    });
  }

  // --- Flagging from the image page: PUT/DELETE requests with the CSRF
  // token (js/http.ts), and the server's reply shown as a toast.
  const FOLDER = '2021-01-01%20Zurich%20trip%200';
  const flagRequests = [];
  page.on('request', (r) => {
    if (r.url().includes('/image/flag/')) {
      flagRequests.push([r.method(), r.headers()['x-xsrf-token'] ?? null]);
    }
  });
  const lastToast = async () => {
    const toast = page.locator('.toast-body').last();
    await toast.waitFor();
    return (await toast.innerText()).trim();
  };
  await page.goto(`${BASE}/folder/${FOLDER}/images/IMG_0001`);
  await check('flag: the flag button sends a PUT with the CSRF token', async () => {
    await page.click('[data-flag="true"]');
    assert.match(await lastToast(), /flagged/i);
    assert.equal(flagRequests.at(-1)[0], 'PUT');
    assert.ok(flagRequests.at(-1)[1], 'CSRF token sent');
  });
  await check('flag: the image shows up in the flagged images', async () => {
    await page.goto(BASE + '/curate/flagged-images');
    await settle(page);
    assert.match(await page.locator('body').innerText(), /IMG_0001/);
  });
  await page.goto(`${BASE}/folder/${FOLDER}/images/IMG_0001`);
  await check('flag: a stale CSRF token asks to reload the page', async () => {
    await page.evaluate(() => {
      document.querySelector('meta[name="csrf-token"]').content = 'stale';
    });
    await page.click('[data-flag="false"]');
    assert.match(await lastToast(), /reload/);
    assert.equal(flagRequests.at(-1)[0], 'DELETE');
  });
  // The 403 above is logged by the browser as a failed resource load.
  const expected403 = errors.findIndex((e) => /status of 403/.test(e));
  if (expected403 >= 0) errors.splice(expected403, 1);
  await page.reload();
  await check('flag: the un-flag button sends a DELETE', async () => {
    await page.click('[data-flag="false"]');
    assert.match(await lastToast(), /flag removed|not flagged/i);
    assert.equal(flagRequests.at(-1)[0], 'DELETE');
  });

  // --- Plot pages: the data comes from the URL in the boot element.
  for (const [url, charts] of [
    ['/stats/cameras', ['#cameraChart', '#trendsChart']],
    ['/stats/lenses', ['#lensChart', '#trendsChart']],
    ['/curate', ['#cameraChart', '#lensChart', '#folderChart', '#yearChart', '#monthChart']],
  ]) {
    await page.goto(BASE + url);
    await check(`${url}: charts drawn`, async () => {
      for (const chart of charts) {
        await page.locator(`${chart} .main-svg`).first().waitFor({ timeout: 10000 });
      }
    });
  }
  for (const [url, link] of [
    ['/stats/cameras', 'a[href*="/stats/camera/"]'],
    ['/stats/lenses', 'a[href*="/stats/lens/"]'],
  ]) {
    await page.goto(BASE + url);
    const href = await page.locator(link).first().getAttribute('href');
    await page.goto(new URL(href, BASE).href);
    await check(`${href}: trends chart drawn`, async () => {
      await page.locator('#trendsChart .main-svg').first().waitFor({ timeout: 10000 });
    });
  }

  // --- The viewer: boot data, navigation (image info requests), the
  // navigation bar, flagging and random images.
  const navText = () => page.locator('#navtext3').innerText();
  const message = async () => (await page.locator('#messageBox').innerText()).trim();
  await page.goto(`${BASE}/view/${FOLDER}/IMG_0002`);
  await settle(page);
  await page.screenshot({ path: path.join(out, 'viewer.png') });
  await check('viewer: boots, sized to the page', async () => {
    assert.equal(await navText(), 'IMG_0002');
    const size = await page.$eval('#imageCanvas', (c) => [c.width, c.height]);
    assert.ok(size[0] > 100 && size[1] > 100, `canvas size ${size}`);
    assert.equal(await page.$eval('footer', (f) => f.style.display), 'none');
  });
  await check('viewer: next image updates the URL and the navigation bar', async () => {
    await page.click('#imageNext');
    await page.waitForURL(/IMG_0003/);
    await page.waitForFunction(
      () => document.getElementById('navtext3').textContent === 'IMG_0003',
    );
    assert.match(await page.getAttribute('#navlink3', 'href'), /IMG_0003/);
  });
  await check('viewer: flag and un-flag via the keyboard', async () => {
    await page.keyboard.press('x');
    await page.waitForFunction(() => /flagged/i.test(document.getElementById('messageBox').textContent));
    assert.equal(flagRequests.at(-1)[0], 'PUT');
    assert.ok(flagRequests.at(-1)[1], 'CSRF token sent');
    await page.keyboard.press('n');
    await page.waitForTimeout(500);
    assert.equal(flagRequests.at(-1)[0], 'DELETE');
    assert.ok((await message()).length > 0);
  });
  await check('viewer: random image', async () => {
    const before = page.url();
    await page.click('#imageRand');
    await page.waitForURL((u) => u.href !== before && u.pathname.startsWith('/view/'), { timeout: 10000 });
  });

  console.log('console errors:', errors.length ? errors : 'none');
  if (errors.length) failures++;
  console.log(failures ? `${failures} FAILED` : 'ALL OK');
  await browser.close();
  process.exitCode = failures ? 1 : 0;
})();
