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

// Checks a public-site build (`make demo-release`), as an anonymous
// visitor and as a logged in user: what anonymous visitors can reach,
// and that they don't see the pictures' problems, the storage paths or
// the tools' error output, while logged in users still do; see
// README.md.
//
// Usage: node e2e/public-site.cjs WORKDIR
//
// WORKDIR is the one given to setup.sh and add-problem.sh.

const fs = require('fs');
const { chromium, request } = require('playwright');
const { BASE, collectErrors, login } = require('./lib.cjs');

const WORK = process.argv[2];
if (!WORK) {
  console.error(`Usage: node ${process.argv[1]} WORKDIR`);
  process.exit(2);
}
const LIB = fs.realpathSync(WORK + '/lib');
const FOLDER = '2024-12-01 Broken 99';
const FURL = '/folder/' + encodeURIComponent(FOLDER);
const PROBLEM = 'JPEG format error';

let failures = 0;
function check(name, ok, detail) {
  const extra = !ok && detail !== undefined ? ` -- ${detail}` : '';
  console.log(`${ok ? 'ok  ' : 'FAIL'} ${name}${extra}`);
  if (!ok) failures++;
}

/** Gets a page, without following redirects. */
async function get(ctx, path) {
  const r = await ctx.get(BASE + path, { maxRedirects: 0 });
  return { status: r.status(), headers: r.headers(), body: await r.text() };
}

/** Waits (up to three minutes) for the scan to find the problem folder. */
async function waitForScan(ctx) {
  for (let i = 0; i < 90; i++) {
    const r = await get(ctx, '/search/folders/list');
    if (r.status === 200 && r.body.includes(FOLDER)) return;
    await new Promise((res) => setTimeout(res, 2000));
  }
  throw new Error(
    'The problem folder not found after three minutes; is the scan done?',
  );
}

async function anonymous() {
  console.log('== anonymous visitor');
  const ctx = await request.newContext();
  await waitForScan(ctx);

  let r = await get(ctx, '/');
  check('home page is public', r.status === 200, r.status);
  check('home links to cameras', r.body.includes('/list/cameras'));
  check('home does not link to problems', !r.body.includes('/list/problems'));
  check('home: no storage paths', !r.body.includes(LIB));
  check(
    'responses are not cached by shared caches',
    (r.headers['cache-control'] || '').includes('private'),
    r.headers['cache-control'],
  );

  r = await get(ctx, '/curate');
  check('curate is public', r.status === 200, r.status);
  check('curate: no problem text', !r.body.includes(PROBLEM));
  check(
    'curate: no problems block',
    !r.body.includes('/list/problems') &&
      !r.body.includes('No pictures with problems'),
  );
  check('curate: has the other blocks', r.body.includes('Flash mode'));

  r = await get(ctx, FURL);
  check('folder page is public', r.status === 200, r.status);
  check('folder: no storage paths', !r.body.includes(LIB));
  check('folder: relative paths', r.body.includes('source: 2024/' + FOLDER));

  r = await get(ctx, FURL + '/images/IMG_BAD');
  check('image page is public', r.status === 200, r.status);
  check('image: no exif warnings', !r.body.includes(PROBLEM));
  check('image: no storage paths', !r.body.includes(LIB));
  check('image: has the exif table', r.body.includes('MIME type'));

  // A preview that fails shows a generic error, not the tool's output.
  r = await get(
    ctx,
    `/image/bytes/${encodeURIComponent(FOLDER)}/IMG_BAD?res=640`,
  );
  check(
    'failed preview: generic error image',
    r.status === 200 &&
      r.headers['content-type'] === 'image/svg+xml' &&
      r.body.includes('could not be read') &&
      !r.body.includes('/dev/fd') &&
      !r.body.includes(LIB),
    `${r.status} ${r.headers['content-type']}`,
  );

  r = await get(ctx, '/list/problems');
  check('problems list refused (403)', r.status === 403, r.status);
  check('the refusal names the problem symbol', r.body.includes('problem'));
  r = await get(ctx, '/list/cameras');
  check('cameras list is public', r.status === 200, r.status);

  const searches = [
    '/search/images/list',
    '/search/folders/list',
    '/search/images/grid/0',
    '/search/images/view',
    '/search',
    '/image/random',
  ];
  for (const q of [
    'problem=~jpeg',
    'no-problem=',
    'country=Japan&problem=~jpeg&or=',
  ]) {
    for (const route of searches) {
      r = await get(ctx, `${route}?${q}`);
      check(`search ${route}?${q} refused`, r.status === 403, r.status);
    }
  }
  r = await get(ctx, '/search/images/list?country=Japan');
  check(
    'other searches work',
    r.status === 200 && r.body.includes('IMG_'),
    r.status,
  );

  for (const q of ['jpeg', 'problem:jpeg', 'Kyoto']) {
    r = await get(ctx, '/search/quick?q=' + encodeURIComponent(q));
    const loc = r.headers['location'] || '';
    check(
      `quick search "${q}" redirects without problems`,
      r.status === 303 && !loc.includes('problem='),
      `${r.status} ${loc}`,
    );
  }

  for (const p of [
    '/settings',
    '/status',
    '/status/errors',
    '/curate/flagged-images',
    '/curate/flagged-images/text',
  ]) {
    r = await get(ctx, p);
    check(
      `${p} needs a login`,
      r.status === 303 && (r.headers['location'] || '').includes('/auth/login'),
      `${r.status} ${r.headers['location']}`,
    );
  }
  await ctx.dispose();
}

async function browserPages(browser) {
  console.log('== browser pages (anonymous)');
  const page = await browser.newPage();
  const errors = collectErrors(page);
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
    const resp = await page.goto(BASE + p, { waitUntil: 'networkidle' });
    check(`page ${p} loads`, resp.status() === 200, resp.status());
  }
  // The viewer, on the first matching image.
  const resp = await page.goto(BASE + '/search/images/view?country=Japan', {
    waitUntil: 'networkidle',
  });
  check(
    'viewer loads',
    resp.status() === 200 && page.url().includes('/view/'),
    `${resp.status()} ${page.url()}`,
  );
  check('no console errors', errors.length === 0, errors.join('\n     '));
  await page.close();
}

async function loggedIn(browser) {
  console.log('== logged in user');
  const page = await browser.newPage();
  await login(page);
  const ctx = page.context().request;

  let r = await get(ctx, '/');
  check('home links to problems', r.body.includes('/list/problems'));
  r = await get(ctx, '/curate');
  check('curate shows the problem', r.body.includes(PROBLEM));
  r = await get(ctx, '/list/problems');
  check(
    'problems list works',
    r.status === 200 && r.body.includes(PROBLEM),
    r.status,
  );
  r = await get(ctx, FURL + '/images/IMG_BAD');
  check('image shows the exif warning', r.body.includes(PROBLEM));
  check('image shows full paths', r.body.includes(LIB));
  r = await get(ctx, '/search/images/list?problem=~jpeg');
  check(
    'problem search works',
    r.status === 200 && r.body.includes('IMG_BAD'),
    r.status,
  );
  r = await get(ctx, '/search/quick?q=jpeg');
  check(
    'quick search finds problems',
    (r.headers['location'] || '').includes('problem='),
    r.headers['location'],
  );
  r = await get(ctx, '/settings');
  check('settings work', r.status === 200, r.status);
  await page.close();
}

(async () => {
  await anonymous();
  const browser = await chromium.launch();
  try {
    await browserPages(browser);
    await loggedIn(browser);
  } finally {
    await browser.close();
  }
  console.log(failures ? `${failures} FAILED` : 'all ok');
  process.exit(failures ? 1 : 0);
})();
