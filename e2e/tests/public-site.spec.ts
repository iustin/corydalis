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

// The public-site build (the "public" project): anonymous visitors can
// browse the public pages, but don't see the pictures' problems, the
// storage paths or the tools' error output (in failed previews);
// searches by problem and the problems list are refused; quick searches
// don't use problems; the settings, status and flagged images pages
// need a login. A logged in user still sees all these. Uses the library
// made by setup.sh plus add-problem.sh.

import fs from 'node:fs';
import { request, type APIRequestContext } from '@playwright/test';
import { test, expect, anonymous, gotoTable, groups } from './fixtures';

const FOLDER = '2024-12-01 Broken 99';
const FURL = '/folder/' + encodeURIComponent(FOLDER);
const PROBLEM = 'JPEG format error';

function lib(): string {
  const dir = process.env.CORYDALIS_PUBLIC_LIB;
  if (!dir) throw new Error('Set CORYDALIS_PUBLIC_LIB (run.sh does)');
  return fs.realpathSync(dir);
}

/** Gets a page, without following redirects. */
async function get(ctx: APIRequestContext, path: string) {
  const r = await ctx.get(path, { maxRedirects: 0 });
  return { status: r.status(), headers: r.headers(), body: await r.text() };
}

test.describe('anonymous visitor', () => {
  test.use(anonymous);
  let ctx: APIRequestContext;
  test.beforeAll(async ({}, testInfo) => {
    test.setTimeout(200_000); // longer than the scan wait below
    ctx = await request.newContext({ baseURL: testInfo.project.use.baseURL });
    // The problem folder is added last, so wait until the scan found it.
    await expect
      .poll(async () => (await get(ctx, '/search/folders/list')).body, {
        timeout: 180_000,
        intervals: [2000],
        message: 'scan found the problem folder',
      })
      .toContain(FOLDER);
  });
  test.afterAll(async () => {
    await ctx.dispose();
  });

  test('the home page is public, without problems or paths', async () => {
    const r = await get(ctx, '/');
    expect(r.status).toBe(200);
    expect(r.body).toContain('/list/cameras');
    expect(r.body).not.toContain('/list/problems');
    expect(r.body).not.toContain(lib());
    expect(
      r.headers['cache-control'] ?? '',
      'not cached by shared caches',
    ).toContain('private');
  });

  test('curate is public, without the problems block', async () => {
    const r = await get(ctx, '/curate');
    expect(r.status).toBe(200);
    expect(r.body).not.toContain(PROBLEM);
    expect(r.body).not.toContain('/list/problems');
    expect(r.body).not.toContain('No pictures with problems');
    expect(r.body, 'has the other blocks').toContain('Flash mode');
  });

  test('folder and image pages are public, without problems or paths', async () => {
    let r = await get(ctx, FURL);
    expect(r.status).toBe(200);
    expect(r.body).not.toContain(lib());
    expect(r.body, 'relative paths').toContain('source: 2024/' + FOLDER);
    r = await get(ctx, FURL + '/images/IMG_BAD');
    expect(r.status).toBe(200);
    expect(r.body).not.toContain(PROBLEM);
    expect(r.body).not.toContain(lib());
    expect(r.body, 'has the exif table').toContain('MIME type');
  });

  test("a failed preview shows a generic error, not the tool's output", async () => {
    const r = await get(
      ctx,
      `/image/bytes/${encodeURIComponent(FOLDER)}/IMG_BAD?res=640`,
    );
    expect(r.status).toBe(200);
    expect(r.headers['content-type']).toBe('image/svg+xml');
    expect(r.body).toContain('could not be read');
    expect(r.body).not.toContain('/dev/fd');
    expect(r.body).not.toContain(lib());
  });

  test('the problems list is refused, the others are public', async () => {
    let r = await get(ctx, '/list/problems');
    expect(r.status).toBe(403);
    expect(r.body, 'the refusal names the problem symbol').toContain('problem');
    r = await get(ctx, '/list/cameras');
    expect(r.status).toBe(200);
  });

  test('searches by problem are refused, others work', async () => {
    for (const q of [
      'problem=~jpeg',
      'no-problem=',
      'country=Japan&problem=~jpeg&or=',
    ]) {
      for (const route of [
        '/search/images/list',
        '/search/folders/list',
        '/search/images/grid/0',
        '/search/images/view',
        '/search',
        '/image/random',
      ]) {
        expect((await get(ctx, `${route}?${q}`)).status, `${route}?${q}`).toBe(
          403,
        );
      }
    }
    const r = await get(ctx, '/search/images/list?country=Japan');
    expect(r.status).toBe(200);
    expect(r.body).toContain('IMG_');
  });

  test('quick searches redirect without problems', async () => {
    for (const q of ['jpeg', 'problem:jpeg', 'Kyoto']) {
      const r = await get(ctx, '/search/quick?q=' + encodeURIComponent(q));
      expect(r.status, q).toBe(303);
      expect(r.headers['location'] ?? '', q).not.toContain('problem=');
    }
  });

  test('settings, status and flagged images need a login', async () => {
    for (const p of [
      '/settings',
      '/status',
      '/status/errors',
      '/curate/flagged-images',
      '/curate/flagged-images/text',
    ]) {
      const r = await get(ctx, p);
      expect(r.status, p).toBe(303);
      expect(r.headers['location'] ?? '', p).toContain('/auth/login');
    }
  });

  for (const p of [
    '/',
    '/curate',
    FURL,
    FURL + '/images/IMG_GOOD',
    '/search/images/grid/0',
    '/search/folders/list',
    '/stats/cameras',
    '/stats/lenses',
    '/about',
  ]) {
    test(`the browser loads ${p}`, async ({ page }) => {
      const resp = await page.goto(p, { waitUntil: 'networkidle' });
      expect(resp?.status()).toBe(200);
      // Not sent to the login page (which also answers 200).
      expect(new URL(page.url()).pathname).toBe(
        new URL(p, page.url()).pathname,
      );
    });
  }

  test('the browser loads the viewer', async ({ page }) => {
    const resp = await page.goto('/search/images/view?country=Japan', {
      waitUntil: 'networkidle',
    });
    expect(resp?.status()).toBe(200);
    expect(page.url()).toContain('/view/');
  });
});

test.describe('logged in user', () => {
  test('sees the problems and full paths', async ({ page }) => {
    const ctx = page.request;
    let r = await get(ctx, '/');
    expect(r.body).toContain('/list/problems');
    r = await get(ctx, '/curate');
    expect(r.body).toContain(PROBLEM);
    r = await get(ctx, '/list/problems');
    expect(r.status).toBe(200);
    expect(r.body).toContain(PROBLEM);
    r = await get(ctx, FURL + '/images/IMG_BAD');
    expect(r.body, 'the exif warning').toContain(PROBLEM);
    expect(r.body, 'full paths').toContain(lib());
  });

  test('can search by problem', async ({ page }) => {
    let r = await get(page.request, '/search/images/list?problem=~jpeg');
    expect(r.status).toBe(200);
    expect(r.body).toContain('IMG_BAD');
    r = await get(page.request, '/search/quick?q=jpeg');
    expect(r.headers['location'] ?? '').toContain('problem=');
  });

  test('can see the settings', async ({ page }) => {
    expect((await get(page.request, '/settings')).status).toBe(200);
  });

  test('/status/errors: table enhanced, grouped by phase', async ({ page }) => {
    await gotoTable(page, '/status/errors', '#errorlist');
    expect((await groups(page, '#errorlist'))[0]).toMatch(/^\w+ \(\d+\)$/);
  });
});
