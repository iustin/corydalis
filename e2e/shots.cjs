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

// Takes screenshots of the sortable tables on a running Corydalis serving
// the library from setup.sh; see README.md.
//
// Usage: node e2e/shots.cjs OUTDIR

const { chromium } = require('playwright');
const path = require('path');
const { BASE, login, outDir } = require('./lib.cjs');

const out = outDir();

(async () => {
  const browser = await chromium.launch();
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2 });
  await login(page);

  // Screenshot of the table (from the page top), limited in height.
  async function shot(name, sel, maxHeight = 900) {
    const box = await page.locator(sel).boundingBox();
    const height = Math.min(box.y + box.height + 8, maxHeight);
    await page.screenshot({ path: path.join(out, name + '.png'), clip: { x: 0, y: 0, width: 1280, height } });
    console.log('saved', name);
  }
  const go = async (url) => {
    await page.goto(BASE + url);
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(300);
  };
  const header = (sel, n) => page.locator(`${sel} thead tr:first-child th:nth-child(${n}) .sort-button`);

  await go('/search/folders/list');
  await shot('01-folders-default-grouped-by-month', '#folderlist');

  await header('#folderlist', 5).click();
  await page.fill('#folderlist .filter-row td:nth-child(7) input', 'dan, bob');
  await page.waitForTimeout(400);
  await shot('02-folders-by-country-filtered-people', '#folderlist');

  await go('/search/images/list');
  await shot('03-images-grouped-by-folder', '#imagelist');

  await go('/folder/2021-01-01%20Zurich%20trip%200');
  await page.locator('#imagelist .filter-row select').selectOption('processed');
  await page.waitForTimeout(300);
  await page.locator('#imagelist').scrollIntoViewIfNeeded();
  await page.locator('#imagelist').screenshot({ path: path.join(out, '04-folder-page-status-filter.png') });
  console.log('saved 04-folder-page-status-filter');

  await go('/stats/cameras');
  await page.locator('#cameralist').scrollIntoViewIfNeeded();
  await page.locator('#cameralist').screenshot({ path: path.join(out, '05-camera-stats.png') });
  console.log('saved 05-camera-stats');

  await go('/stats/lenses');
  await page.locator('#lenslist').scrollIntoViewIfNeeded();
  await page.locator('#lenslist').screenshot({ path: path.join(out, '06-lens-stats.png') });
  console.log('saved 06-lens-stats');

  await go('/list/people');
  await shot('07-people-list', '#itemlist');

  await go('/status/errors');
  await shot('08-status-errors-grouped-by-phase', '#errorlist');

  // Keyboard: tab to the "Images" sort button and sort with Enter.
  await go('/search/folders/list');
  await header('#folderlist', 4).focus();
  await page.keyboard.press('Enter');
  await page.keyboard.press('Enter');
  await page.waitForTimeout(200);
  await shot('09-folders-keyboard-focus-sorted-desc', '#folderlist', 500);

  await browser.close();
})();
