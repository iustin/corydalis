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

// Smoke test: every page of the application loads for a logged in
// user, with a success status, its navigation bar, and no console
// errors (checked by the fixture).

import { test, expect, FOLDER_URL, type Page } from './fixtures';

const FOLDER = FOLDER_URL.replace('/folder/', '');

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
  '/search?country=Japan',
  `${FOLDER_URL}`,
  `${FOLDER_URL}/images/IMG_0001`,
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

/** Loads the page, and checks the status, location and navigation bar. */
async function loads(page: Page, url: string): Promise<void> {
  const resp = await page.goto(url, { waitUntil: 'networkidle' });
  expect(resp?.status(), 'status').toBe(200);
  // The login page has a navigation bar too, so check that we were not
  // sent there.
  expect(new URL(page.url()).pathname).not.toMatch(/^\/auth\//);
  await expect(page.locator('nav.navbar')).toBeVisible();
}

for (const url of PAGES) {
  test(`${url} loads`, async ({ page }) => {
    await loads(page, url);
  });
}

// These redirect to the first matching image or result.
for (const [url, to] of [
  ['/search/images/view', /^\/view\//],
  ['/search/quick?q=Zurich', /^\/search\/images\/grid\//],
] as const) {
  test(`${url} redirects and loads`, async ({ page }) => {
    await loads(page, url);
    expect(new URL(page.url()).pathname).toMatch(to);
  });
}

for (const [list, prefix] of [
  ['/stats/lenses', '/stats/lens/'],
  ['/stats/cameras', '/stats/camera/'],
]) {
  test(`the first ${prefix} page loads`, async ({ page }) => {
    await page.goto(list);
    const href = await page
      .locator(`a[href*="${prefix}"]`)
      .first()
      .getAttribute('href');
    expect(href).toBeTruthy();
    await loads(page, href!);
  });
}
