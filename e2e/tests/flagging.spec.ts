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

// Flagging from the image page: PUT/DELETE requests with the CSRF token
// (js/http.ts), and the server's reply shown as a toast. The tests run
// in order, as they flag and un-flag the same image.

import { test, expect, FOLDER_URL, type Page } from './fixtures';

const IMAGE = `${FOLDER_URL}/images/IMG_0001`;

/** Clicks, and returns the flag request it sent and the toast text. */
async function flagClick(page: Page, selector: string) {
  const [resp] = await Promise.all([
    page.waitForResponse((r) => r.url().includes('/image/flag/')),
    page.click(selector),
  ]);
  const req = resp.request();
  const toast = page.locator('.toast-body').last();
  await expect(toast).toBeVisible();
  return {
    method: req.method(),
    status: resp.status(),
    token: req.headers()['x-xsrf-token'] ?? null,
    toast: (await toast.innerText()).trim(),
  };
}

test.describe.configure({ mode: 'serial' });

test('the flag button sends a PUT with the CSRF token', async ({ page }) => {
  await page.goto(IMAGE);
  const r = await flagClick(page, '[data-flag="true"]');
  expect(r.method).toBe('PUT');
  expect(r.token, 'CSRF token sent').toBeTruthy();
  expect(r.status).toBe(200);
  expect(r.toast).toBe('Image flagged');
});

test('the image shows up in the flagged images', async ({ page }) => {
  await page.goto('/curate/flagged-images');
  await expect(page.locator('#main')).toContainText('IMG_0001');
});

test('a stale CSRF token asks to reload the page', async ({
  page,
  consoleErrors,
}) => {
  // The refused request is logged by the browser as a failed load.
  consoleErrors.allow(/status of 403/);
  await page.goto(IMAGE);
  await page.evaluate(() => {
    document.querySelector<HTMLMetaElement>(
      'meta[name="csrf-token"]',
    )!.content = 'stale';
  });
  const r = await flagClick(page, '[data-flag="false"]');
  expect(r.method).toBe('DELETE');
  expect(r.toast).toMatch(/reload/);
});

test('the un-flag button sends a DELETE', async ({ page }) => {
  await page.goto(IMAGE);
  const r = await flagClick(page, '[data-flag="false"]');
  expect(r.method).toBe('DELETE');
  expect(r.status).toBe(200);
  expect(r.toast).toBe('Image flag removed');
});
