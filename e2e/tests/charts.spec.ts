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

/** Hovers over the largest bar or point, and expects a label. */
async function hover(page: Page, c: string): Promise<void> {
  // As seen in the plot area: bars on a log axis extend (clipped) far
  // below it.
  await page.locator(`#${c}`).scrollIntoViewIfNeeded();
  const area = (await page.locator(`#${c} .nsewdrag`).first().boundingBox())!;
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
  expect(boxes.length, `${c}: bars or points in view`).toBeGreaterThan(0);
  const box = boxes[0];
  await page.mouse.move(box.x + box.w / 2, box.y + box.h / 2);
  await expect(
    page.locator(`#${c} .hoverlayer .hovertext`).first(),
    `${c}: hover label`,
  ).toBeVisible();
}

/**
 * Goes to the page, and waits for its charts; `charts` null means all
 * the charts on the page (at least one). Returns the chart ids.
 */
async function gotoCharts(
  page: Page,
  url: string,
  charts: string[] | null,
): Promise<string[]> {
  await page.goto(url);
  await expect(page.locator('.js-plotly-plot .main-svg').first()).toBeVisible();
  const ids =
    charts ??
    (await page.$$eval('.js-plotly-plot', (es) => es.map((e) => e.id)));
  for (const c of ids) {
    await expect(page.locator(`#${c} .main-svg`).first()).toBeVisible();
  }
  return ids;
}

/** The URL of the first camera or lens page, from the list page. */
async function firstLink(page: Page, list: string, prefix: string) {
  await page.goto(list);
  const href = await page
    .locator(`a[href*="${prefix}"]`)
    .first()
    .getAttribute('href');
  expect(href, `a link to a ${prefix} page`).toBeTruthy();
  return href!;
}

// The pages, their charts (null: all, found on the page), and the
// charts with the two update menus (stacked/individual,
// absolute/normalized).
const PAGES: [string, string[] | null, string[]][] = [
  ['/stats/cameras', ['cameraChart', 'trendsChart'], ['trendsChart']],
  ['/stats/lenses', ['lensChart', 'trendsChart'], ['trendsChart']],
  [
    '/curate',
    ['cameraChart', 'lensChart', 'folderChart', 'yearChart', 'monthChart'],
    [],
  ],
  ['/stats/camera/', null, ['trendsChart']],
  ['/stats/lens/', null, ['trendsChart']],
];

const LISTS: Record<string, string> = {
  '/stats/camera/': '/stats/cameras',
  '/stats/lens/': '/stats/lenses',
};

for (const [where, charts, withMenus] of PAGES) {
  test.describe(where, () => {
    let ids: string[] = [];
    test.beforeEach(async ({ page }) => {
      // For the camera and lens pages, the first one in their list.
      const url = where.endsWith('/')
        ? await firstLink(page, LISTS[where], where)
        : where;
      ids = await gotoCharts(page, url, charts);
    });

    test('charts drawn, with data', async ({ page }) => {
      for (const c of ids) {
        expect((await layout(page, c)).points, c).toBeGreaterThan(0);
      }
    });

    test('hovering shows a label', async ({ page }) => {
      for (const c of ids) await hover(page, c);
    });

    if (withMenus.length > 0) {
      test('update menus change the bar mode', async ({ page }) => {
        for (const c of withMenus) {
          const headers = page.locator(`#${c} .updatemenu-header-group`);
          await expect(headers, `${c} menus`).toHaveCount(2);
          for (let i = 0; i < 2; i++) {
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
    }

    test('charts follow the window width', async ({ page }) => {
      const wide = await Promise.all(
        ids.map(async (c) => (await layout(page, c)).width),
      );
      await page.setViewportSize({ width: 800, height: 900 });
      for (const [i, c] of ids.entries()) {
        await expect
          .poll(async () => (await layout(page, c)).width, c)
          .toBeLessThan(wide[i]);
      }
    });
  });
}

test('the plot bundle is served', async ({ page }) => {
  const [resp] = await Promise.all([
    page.waitForResponse((r) => /bundle-plot\.js/.test(r.url())),
    page.goto('/stats/cameras'),
  ]);
  expect(resp.status()).toBe(200);
});
