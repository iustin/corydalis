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

// The colour theme (js/theme.ts): the navbar button, the system's
// setting, the choice kept across pages, and the charts following it.

import { test, expect, type Page } from './fixtures';

const theme = (page: Page) =>
  page.evaluate(() => document.documentElement.dataset.bsTheme);

/** Lets the page see a change of the system's colour scheme. */
async function systemScheme(page: Page, colorScheme: 'light' | 'dark') {
  await page.emulateMedia({ colorScheme });
  // The media query's change event comes with the next rendering.
  await page.evaluate(
    () =>
      new Promise((r) => requestAnimationFrame(() => requestAnimationFrame(r))),
  );
}

test('follows the system, then the button, and keeps the choice', async ({
  page,
}) => {
  await systemScheme(page, 'dark');
  await page.goto('/about');
  expect(await theme(page)).toBe('dark'); // automatic
  await systemScheme(page, 'light');
  expect(await theme(page)).toBe('light'); // still automatic
  const button = page.locator('#theme-toggle');
  await button.click();
  expect(await theme(page)).toBe('light');
  await button.click();
  expect(await theme(page)).toBe('dark');
  await systemScheme(page, 'dark');
  await systemScheme(page, 'light');
  expect(await theme(page)).toBe('dark'); // chosen, so not the system's
  // Another page starts with the choice already applied.
  await page.goto('/status');
  expect(await theme(page)).toBe('dark');
  await expect(button).toHaveAttribute('title', /^Theme: dark;/);
  // Back to automatic, which follows the system again.
  await button.click();
  expect(await theme(page)).toBe('light');
});

test('the charts follow the theme', async ({ page }) => {
  await page.emulateMedia({ colorScheme: 'light' });
  await page.goto('/curate');
  const chart = page.locator('.js-plotly-plot').first();
  await expect(chart).toBeVisible();
  const fontColor = () =>
    chart.evaluate(
      (c) =>
        (c as unknown as { layout: { font: { color: string } } }).layout.font
          .color,
    );
  const light = await fontColor();
  // automatic -> light -> dark
  await page.locator('#theme-toggle').click();
  await page.locator('#theme-toggle').click();
  await expect.poll(fontColor).not.toBe(light);
});
