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

// Shared fixtures and helpers of the e2e tests:
//
// - every test runs logged in (the login happens once per worker and
//   project, after waiting for the server's scan), unless it uses
//   `anonymous`;
// - every test fails if the browser logged a console error or a page
//   error or warning, except for the ones it expects
//   (`consoleErrors.allow`).

import fs from 'node:fs';
import path from 'node:path';
import {
  test as base,
  expect,
  type Locator,
  type Page,
} from '@playwright/test';

export { expect, type Page };

/** The test user's password, for the given project. */
function password(project: string): string {
  const env =
    project === 'public'
      ? 'CORYDALIS_PUBLIC_PASSWORD_FILE'
      : 'CORYDALIS_PASSWORD_FILE';
  const file = process.env[env];
  if (!file) throw new Error(`Set ${env} (run.sh does)`);
  return fs.readFileSync(file, 'utf8').trim();
}

/**
 * The number of folders in each project's library (see setup.sh; the
 * public one also has those of add-problem.sh and add-movie.sh).
 */
const FOLDERS: Record<string, number> = { private: 20, public: 22 };

/** Logs in, then waits (up to two minutes) for the scan to finish. */
async function login(page: Page, project: string): Promise<void> {
  await page.goto('/auth/login');
  await page.fill('#floatingUsername', process.env.CORYDALIS_USER ?? 'tester');
  await page.fill('#floatingPassword', password(project));
  await Promise.all([
    page.waitForURL((u) => !u.pathname.startsWith('/auth/')),
    page.click('form[action$="/login"] [type=submit]'),
  ]);
  await expect
    .poll(
      async () => {
        await page.goto('/search/folders/list');
        return page.locator('#folderlist tbody tr:not(.group-header)').count();
      },
      { timeout: 120_000, intervals: [2000], message: 'scan finished' },
    )
    .toBe(FOLDERS[project]);
}

type ConsoleErrors = {
  /** Doesn't fail the test for (one) console error matching `re`. */
  allow(re: RegExp): void;
};

export const test = base.extend<
  { consoleErrors: ConsoleErrors },
  { workerStorageState: string }
>({
  // Logged in by default; anonymous tests use `anonymous` below.
  storageState: ({ workerStorageState }, use) => use(workerStorageState),
  workerStorageState: [
    async ({ browser }, use, workerInfo) => {
      const project = workerInfo.project.name;
      const file = path.join(
        workerInfo.project.outputDir,
        `.auth-${project}-${workerInfo.workerIndex}.json`,
      );
      const context = await browser.newContext({
        baseURL: workerInfo.project.use.baseURL,
        storageState: undefined,
      });
      const page = await context.newPage();
      await login(page, project);
      await context.storageState({ path: file });
      await context.close();
      await use(file);
    },
    // Longer than the scan wait in login().
    { scope: 'worker', timeout: 150_000 },
  ],
  consoleErrors: [
    async ({ page }, use) => {
      const errors: string[] = [];
      page.on('pageerror', (e) => errors.push(`[${page.url()}] ${e.message}`));
      page.on('console', (m) => {
        if (m.type() === 'error' || m.type() === 'warning') {
          errors.push(`[${page.url()}] ${m.type()}: ${m.text()}`);
        }
      });
      const allowed: RegExp[] = [];
      await use({ allow: (re) => allowed.push(re) });
      // Let the requests started at the end of the test finish (e.g. the
      // next image's), so that their errors are seen. Best effort: pages
      // that keep polling never become idle.
      await page
        .waitForLoadState('networkidle', { timeout: 5000 })
        .catch(() => {});
      const unexpected = errors.filter((e) => {
        const i = allowed.findIndex((re) => re.test(e));
        if (i < 0) return true;
        allowed.splice(i, 1);
        return false;
      });
      expect(unexpected, 'browser console errors').toEqual([]);
    },
    { auto: true },
  ],
});

/** Test options for anonymous visitors (no login). */
export const anonymous = { storageState: { cookies: [], origins: [] } };

export const FOLDER = '2021-01-01 Zurich trip 0';
export const FOLDER_URL = `/folder/${encodeURIComponent(FOLDER)}`;

/** Goes to a page with a sortable table, and waits for it to be enhanced. */
export async function gotoTable(
  page: Page,
  url: string,
  table: string,
): Promise<void> {
  await page.goto(url);
  await expect(
    page.locator(`${table} thead .sort-button`).first(),
  ).toBeVisible();
}

/** Visible data rows (not group headers), as arrays of cell texts. */
export function dataRows(page: Page, table: string): Promise<string[][]> {
  return page.$$eval(`${table} tbody tr:not(.group-header)`, (rs) =>
    rs.map((r) =>
      [...(r as HTMLTableRowElement).cells].map((c) => c.innerText.trim()),
    ),
  );
}

/** The group header texts. */
export function groups(page: Page, table: string): Promise<string[]> {
  return page.$$eval(`${table} tbody tr.group-header`, (rs) =>
    rs.map((r) => (r as HTMLElement).innerText.trim()),
  );
}

/** The aria-sort attributes of the header cells. */
export function ariaSort(
  page: Page,
  table: string,
): Promise<(string | null)[]> {
  return page.$$eval(`${table} thead tr:first-child th`, (ths) =>
    ths.map((t) => t.getAttribute('aria-sort')),
  );
}

/** The sort button of the n-th column (from 1). */
export function header(page: Page, table: string, n: number): Locator {
  return page.locator(
    `${table} thead tr:first-child th:nth-child(${n}) .sort-button`,
  );
}

/**
 * Runs `action` (which changes a filter), and waits until the table
 * re-rendered its rows: text filters apply after a short delay, and a
 * filter that doesn't change the row count is otherwise invisible. The
 * re-rendering replaces all the rows, so it removes a marker row added
 * here.
 */
export async function rerender(
  page: Page,
  table: string,
  action: () => Promise<unknown>,
): Promise<void> {
  const marker = page.locator(`${table} tbody tr[data-e2e-marker]`);
  await page.$eval(`${table} tbody`, (body) => {
    const tr = document.createElement('tr');
    tr.dataset.e2eMarker = '';
    body.append(tr);
  });
  await action();
  await expect(marker).toHaveCount(0);
}

/** Fills the filter of the n-th column (from 1); returns the row count. */
export async function filterCount(
  page: Page,
  table: string,
  col: number,
  value: string,
): Promise<number> {
  const input = page.locator(`${table} .filter-row td:nth-child(${col}) input`);
  await rerender(page, table, () => input.fill(value));
  return (await dataRows(page, table)).length;
}
