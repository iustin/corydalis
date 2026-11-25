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

// The sortable tables (js/tablesort.ts) on the real pages: sorting,
// grouping, filters and keyboard use. The sorting and filtering rules
// themselves are unit tested (js/tablesort*.test.ts); these check that
// the templates give the tables what the code expects. The counts
// depend on the library made by setup.sh.

import {
  test,
  expect,
  ariaSort,
  dataRows,
  filterCount,
  FOLDER_URL,
  gotoTable,
  groups,
  header,
  rerender,
  type Page,
} from './fixtures';

const coll = new Intl.Collator(undefined, {
  numeric: true,
  sensitivity: 'base',
});
const txt = (a: string, b: string) => coll.compare(a, b);
const sortedBy = (
  rows: string[][],
  col: number,
  cmp: (a: string, b: string) => number,
) => rows.every((r, i) => i === 0 || cmp(rows[i - 1][col], r[col]) <= 0);

/**
 * Clicks the header of a column with distinct texts twice, and checks
 * that the rows follow aria-sort each time (the first click's direction
 * depends on the column's settings).
 */
async function checkSortToggle(page: Page, table: string, n: number) {
  const texts = async () => (await dataRows(page, table)).map((r) => r[n - 1]);
  await header(page, table, n).click();
  const first = await texts();
  expect(new Set(first).size, 'distinct texts').toBe(first.length);
  const dir = (await ariaSort(page, table))[n - 1];
  const sorted = [...first].sort(txt);
  expect(first).toEqual(dir === 'ascending' ? sorted : sorted.reverse());
  await header(page, table, n).click();
  expect((await ariaSort(page, table))[n - 1]).toBe(
    dir === 'ascending' ? 'descending' : 'ascending',
  );
  expect(await texts()).toEqual([...first].reverse());
}

test.describe('folder list', () => {
  const F = '#folderlist';
  test.beforeEach(async ({ page }) => {
    await gotoTable(page, '/search/folders/list', F);
  });

  test('all 20 rows, grouped by month of first picture', async ({ page }) => {
    const rows = await dataRows(page, F);
    expect(rows).toHaveLength(20);
    expect(sortedBy(rows, 2, txt), 'sorted by first picture').toBe(true);
    const g = await groups(page, F);
    expect(g[0]).toBe('Jan 2021 (2)');
    const total = g.reduce((s, x) => s + Number(/\((\d+)\)$/.exec(x)![1]), 0);
    expect(total, 'group counts add up').toBe(20);
  });

  test('aria-sort reflects the initial sort list', async ({ page }) => {
    expect(await ariaSort(page, F)).toEqual([
      null,
      'ascending',
      'ascending',
      'none',
      'none',
      'none',
      'none',
    ]);
  });

  test('tfoot totals row untouched', async ({ page }) => {
    const t = await page.$$eval(`${F} tfoot tr`, (rs) =>
      rs.map((r) => (r as HTMLElement).innerText.replace(/\s+/g, ' ').trim()),
    );
    expect(t).toHaveLength(1);
    expect(t[0]).toMatch(/^Totals 120/);
  });

  test('clicking Name sorts by it, both ways, without groups', async ({
    page,
  }) => {
    await checkSortToggle(page, F, 2);
    expect(await groups(page, F)).toEqual([]);
  });

  test('clicking Images marks it sorted, without groups', async ({ page }) => {
    // All folders have 6 images, so the order itself can't be checked.
    await header(page, F, 4).click();
    expect((await ariaSort(page, F))[3]).toBe('ascending');
    expect(await groups(page, F)).toEqual([]);
    await header(page, F, 4).click();
    expect((await ariaSort(page, F))[3]).toBe('descending');
  });

  test('clicking Countries groups by country; shift-click adds a secondary sort', async ({
    page,
  }) => {
    await header(page, F, 5).click();
    expect(await groups(page, F)).toEqual([
      'Italy (4)',
      'Japan (4)',
      'Norway (4)',
      'Romania (4)',
      'Switzerland (4)',
    ]);
    await header(page, F, 2).click({ modifiers: ['Shift'] });
    const a = await ariaSort(page, F);
    expect(a[4]).toBe('ascending');
    expect(a[1]).toBe('ascending');
    const italy = async () =>
      (await dataRows(page, F))
        .filter((r) => r[4] === 'Italy')
        .map((r) => r[1]);
    const asc = await italy();
    expect(asc).toHaveLength(4);
    expect(asc).toEqual([...asc].sort(txt));
    // A second shift-click reverses the secondary sort only.
    await header(page, F, 2).click({ modifiers: ['Shift'] });
    expect((await ariaSort(page, F))[4]).toBe('ascending');
    expect(await italy()).toEqual([...asc].reverse());
  });

  test('text filter (case-insensitive substring), with groups', async ({
    page,
  }) => {
    await header(page, F, 5).click(); // group by country
    expect(await filterCount(page, F, 6, 'KYO')).toBe(4);
    const rows = await dataRows(page, F);
    expect(rows.every((r) => r[5] === 'Kyoto')).toBe(true);
    expect(await groups(page, F)).toEqual(['Japan (4)']);
    expect(await filterCount(page, F, 6, '')).toBe(20);
  });

  test('icon column has no filter', async ({ page }) => {
    await expect(
      page.locator(`${F} .filter-row td:nth-child(1) *`),
    ).toHaveCount(0);
  });

  test('keyboard sorting via focus + Enter/Space', async ({ page }) => {
    await header(page, F, 4).focus();
    await page.keyboard.press('Enter');
    expect((await ariaSort(page, F))[3]).toBe('ascending');
    await page.keyboard.press('Space');
    expect((await ariaSort(page, F))[3]).toBe('descending');
  });

  test('numeric operators: placeholder only on numeric columns', async ({
    page,
  }) => {
    const ph = await page.$$eval(`${F} .filter-row input`, (is) =>
      is.map((i) => (i as HTMLInputElement).placeholder),
    );
    expect(ph).toEqual(['', '', 'e.g. >10', '', '', '']);
  });

  test('numeric operators on Images (all folders have 6)', async ({ page }) => {
    for (const [value, count] of [
      ['>5', 20],
      ['>6', 0],
      ['>= 6', 20],
      ['≤5', 0],
      ['=6', 20],
      ['/=6', 0],
      ['6', 20], // a bare number is a substring
      ['7', 0],
      ['>x', 0], // an invalid operator is text
      ['', 20],
    ] as const) {
      expect(await filterCount(page, F, 4, value), `filter "${value}"`).toBe(
        count,
      );
    }
  });

  test('numeric operators: text columns treat ">" as text', async ({
    page,
  }) => {
    expect(await filterCount(page, F, 5, '>5')).toBe(0);
  });
});

test.describe('image list', () => {
  const I = '#imagelist';
  test.beforeEach(async ({ page }) => {
    await gotoTable(page, '/search/images/list', I);
  });

  test('120 rows grouped by folder', async ({ page }) => {
    expect(await dataRows(page, I)).toHaveLength(120);
    const g = await groups(page, I);
    expect(g).toHaveLength(20);
    expect(g[0]).toBe('2021-01-01 Zurich trip 0 (6)');
  });

  test('no status column, so no select filter', async ({ page }) => {
    await expect(page.locator(`${I} .filter-row select`)).toHaveCount(0);
  });

  test('thumbnails column not sortable', async ({ page }) => {
    await expect(
      page.locator(`${I} thead tr:first-child th:nth-child(1) .sort-button`),
    ).toHaveCount(0);
  });
});

test('folder page: status select filter (on data-text)', async ({ page }) => {
  const I = '#imagelist';
  await gotoTable(page, FOLDER_URL, I);
  const sel = page.locator(`${I} .filter-row select`);
  await expect(sel).toHaveCount(1);
  const opts = await sel
    .locator('option')
    .evaluateAll((os) =>
      os.map((o) => [(o as HTMLOptionElement).value, o.textContent]),
    );
  expect(
    opts.length,
    'at least two statuses plus the empty option',
  ).toBeGreaterThanOrEqual(3);
  const value = opts[1][0]!;
  await rerender(page, I, () => sel.selectOption(value));
  const statuses = await page.$$eval(
    `${I} tbody tr:not(.group-header) td:last-child`,
    (tds) => tds.map((td) => (td as HTMLElement).dataset.text ?? ''),
  );
  expect(statuses.length).toBeGreaterThan(0);
  expect(statuses.length).toBeLessThan(6);
  expect(statuses.every((t) => t.toLowerCase() === value)).toBe(true);
  await rerender(page, I, () => sel.selectOption(''));
  expect(await dataRows(page, I)).toHaveLength(6);
});

// The counts are the same for all cameras and lenses in the sample
// library, so sort by name to check the order.
for (const [url, table, sortedCol] of [
  ['/stats/cameras', '#cameralist', 2],
  ['/stats/lenses', '#lenslist', 4],
] as const) {
  test(`${url}: sorted, sortable, with filters`, async ({ page }) => {
    await gotoTable(page, url, table);
    expect((await dataRows(page, table)).length).toBeGreaterThanOrEqual(3);
    expect((await ariaSort(page, table))[sortedCol]).toBe('descending');
    await checkSortToggle(page, table, 1);
    expect(
      await page
        .locator(`${table} .filter-row input, ${table} .filter-row select`)
        .count(),
    ).toBeGreaterThan(0);
    expect(await groups(page, table)).toEqual([]);
  });
}

test('/stats/lenses: type select shows labels, filters on data-text', async ({
  page,
}) => {
  const L = '#lenslist';
  await gotoTable(page, '/stats/lenses', L);
  const sel = page.locator(`${L} .filter-row select`);
  const opts = await sel
    .locator('option')
    .evaluateAll((os) =>
      os.map((o) => [(o as HTMLOptionElement).value, o.textContent]),
    );
  expect(opts.some(([v, t]) => v !== t && v !== '')).toBe(true);
  const before = (await dataRows(page, L)).length;
  await rerender(page, L, () => sel.selectOption(opts[1][0]!));
  const types = await page.$$eval(`${L} tbody tr td:nth-child(3)`, (tds) =>
    tds.map((td) => (td as HTMLElement).dataset.text),
  );
  expect(types.length).toBeGreaterThan(0);
  // All the sample lenses have the same type, so the filter can't
  // remove rows here; it still has to match on the data-text values.
  expect(types.length).toBe(before);
  expect(types.every((t) => t === opts[1][0])).toBe(true);
});

test('/stats/cameras: numeric operators, and empty cells', async ({ page }) => {
  const C = '#cameralist';
  await gotoTable(page, '/stats/cameras', C);
  expect(await filterCount(page, C, 3, '=0')).toBe(3);
  expect(await filterCount(page, C, 3, '>0')).toBe(0);
  await filterCount(page, C, 3, '');
  expect(await filterCount(page, C, 4, '!=0'), 'empty cells never match').toBe(
    0,
  );
  expect(await filterCount(page, C, 4, '')).toBe(3);
  expect(await filterCount(page, C, 2, '>=40')).toBe(3);
  expect(await filterCount(page, C, 2, '<40')).toBe(0);
});

for (const [url, table] of [
  [FOLDER_URL, '#imagelist'],
  ['/list/people', '#itemlist'],
  ['/list/countries', '#itemlist'],
]) {
  test(`${url}: table enhanced`, async ({ page }) => {
    await gotoTable(page, url, table);
    expect((await dataRows(page, table)).length).toBeGreaterThan(0);
    await expect(page.locator(`${table} .filter-row`)).toHaveCount(1);
  });
}
