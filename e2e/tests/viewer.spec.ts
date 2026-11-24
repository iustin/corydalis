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

// The viewer (js/viewer-app.ts): boot data, navigation (image info
// requests), the navigation bar, flagging and random images.

import { test, expect, FOLDER_URL } from './fixtures';

const VIEW = FOLDER_URL.replace('/folder/', '/view/');

test.beforeEach(async ({ page }) => {
  await page.goto(`${VIEW}/IMG_0002`);
  await expect(page.locator('#navtext3')).toHaveText('IMG_0002');
});

test('boots, sized to the page', async ({ page }) => {
  const [w, h] = await page.$eval('#imageCanvas', (c) => [
    (c as HTMLCanvasElement).width,
    (c as HTMLCanvasElement).height,
  ]);
  expect(w).toBeGreaterThan(100);
  expect(h).toBeGreaterThan(100);
  await expect(page.locator('footer')).toBeHidden();
});

test('next image updates the URL and the navigation bar', async ({ page }) => {
  await page.click('#imageNext');
  await page.waitForURL(/IMG_0003/);
  await expect(page.locator('#navtext3')).toHaveText('IMG_0003');
  await expect(page.locator('#navlink3')).toHaveAttribute('href', /IMG_0003/);
});

test('flag and un-flag via the keyboard', async ({ page }) => {
  const flag = (key: string) =>
    Promise.all([
      page.waitForResponse((r) => r.url().includes('/image/flag/')),
      page.keyboard.press(key),
    ]).then(([r]) => r);
  let r = await flag('x');
  expect(r.request().method()).toBe('PUT');
  expect(r.request().headers()['x-xsrf-token'], 'CSRF token sent').toBeTruthy();
  await expect(page.locator('#messageBox')).toContainText(/flagged/i);
  r = await flag('n');
  expect(r.request().method()).toBe('DELETE');
  expect(r.ok()).toBe(true);
  await expect(page.locator('#messageBox')).not.toHaveText('');
});

test('random image', async ({ page }) => {
  const before = page.url();
  await page.click('#imageRand');
  await page.waitForURL(
    (u) => u.href !== before && u.pathname.startsWith('/view/'),
  );
});
