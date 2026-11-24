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

// The charts on the plot pages (js/charts.ts, plotly): drawn with data,
// hover labels, the update menus, and responsive resizing. The chart
// data shaping is unit tested (js/charts.test.ts); to compare the
// charts in detail before and after a change (e.g. a plotly update),
// use plots.cjs (see README.md).

import { test, expect, type Page } from './fixtures';

/** The chart's layout and data, from plotly's state. */
function layout(page: Page, id: string) {
  return page.evaluate((id) => {
    const gd = document.getElementById(id) as unknown as {
      _fullLayout: { width: number; barmode: string; barnorm: string };
      _fullData: { x?: unknown[]; labels?: unknown[] }[];
    };
    const fl = gd._fullLayout;
    return {
      width: fl.width,
      mode: `${fl.barmode}/${fl.barnorm}`,
      points: gd._fullData.reduce(
        (n, t) => n + (t.x ?? t.labels ?? []).length,
        0,
      ),
    };
  }, id);
}

const PAGES: [string, string[]][] = [
  ['/stats/cameras', ['cameraChart', 'trendsChart']],
  ['/stats/lenses', ['lensChart', 'trendsChart']],
  [
    '/curate',
    ['cameraChart', 'lensChart', 'folderChart', 'yearChart', 'monthChart'],
  ],
];

for (const [url, charts] of PAGES) {
  test.describe(url, () => {
    test.beforeEach(async ({ page }) => {
      await page.goto(url);
      for (const c of charts) {
        await expect(page.locator(`#${c} .main-svg`).first()).toBeVisible();
      }
    });

    test('charts drawn, with data', async ({ page }) => {
      for (const c of charts) {
        expect((await layout(page, c)).points, c).toBeGreaterThan(0);
      }
    });

    test('hovering shows a label', async ({ page }) => {
      for (const c of charts) {
        // The largest bar or point, as seen in the plot area: bars on a
        // log axis extend (clipped) far below it.
        await page.locator(`#${c}`).scrollIntoViewIfNeeded();
        const area = (await page
          .locator(`#${c} .nsewdrag`)
          .first()
          .boundingBox())!;
        const targets = page.locator(
          `#${c} .bars .point path, #${c} .scatterlayer .point`,
        );
        const boxes = (
          await Promise.all((await targets.all()).map((t) => t.boundingBox()))
        )
          .filter((b) => b !== null)
          .map((b) => {
            const x = Math.max(b.x, area.x);
            const y = Math.max(b.y, area.y);
            const w = Math.min(b.x + b.width, area.x + area.width) - x;
            const h = Math.min(b.y + b.height, area.y + area.height) - y;
            return { x, y, w, h };
          })
          .filter((b) => b.w > 0 && b.h > 0)
          .sort((a, b) => b.w * b.h - a.w * a.h);
        expect(boxes.length, c).toBeGreaterThan(0);
        const box = boxes[0];
        await page.mouse.move(box.x + box.w / 2, box.y + box.h / 2);
        await expect(
          page.locator(`#${c} .hoverlayer .hovertext`).first(),
          c,
        ).toBeVisible();
      }
    });

    test('update menus change the bar mode', async ({ page }) => {
      for (const c of charts) {
        const headers = page.locator(`#${c} .updatemenu-header-group`);
        const n = await headers.count();
        for (let i = 0; i < n; i++) {
          const before = (await layout(page, c)).mode;
          await headers.nth(i).click();
          const buttons = page.locator(`#${c} .updatemenu-dropdown-button`);
          await expect(buttons.first()).toBeVisible();
          expect(await buttons.count(), `${c} menu ${i}`).toBeGreaterThan(1);
          await buttons.nth(1).click();
          await expect
            .poll(async () => (await layout(page, c)).mode, `${c} menu ${i}`)
            .not.toBe(before);
        }
      }
    });

    test('charts follow the window width', async ({ page }) => {
      const wide = await Promise.all(
        charts.map(async (c) => (await layout(page, c)).width),
      );
      await page.setViewportSize({ width: 800, height: 900 });
      for (const [i, c] of charts.entries()) {
        await expect
          .poll(async () => (await layout(page, c)).width, c)
          .toBeLessThan(wide[i]);
      }
    });
  });
}

for (const [list, prefix] of [
  ['/stats/cameras', '/stats/camera/'],
  ['/stats/lenses', '/stats/lens/'],
]) {
  test(`the first ${prefix} page draws its trends chart`, async ({ page }) => {
    await page.goto(list);
    const href = await page
      .locator(`a[href*="${prefix}"]`)
      .first()
      .getAttribute('href');
    await page.goto(href!);
    await expect(page.locator('#trendsChart .main-svg').first()).toBeVisible();
    expect((await layout(page, 'trendsChart')).points).toBeGreaterThan(0);
  });
}

test('the plot bundle is served', async ({ page }) => {
  const [resp] = await Promise.all([
    page.waitForResponse((r) => /bundle-plot\.js/.test(r.url())),
    page.goto('/stats/cameras'),
  ]);
  expect(resp.status()).toBe(200);
});
