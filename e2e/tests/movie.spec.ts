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

// Movies in the viewer: the first frame is shown with a note, and
// playing switches to the video element, served by the server. The
// movies are added by add-movie.sh: in the private project's library,
// next to a picture (which is the frame shown); in the public one, on
// its own (so the server extracts its first frame with ffmpeg).

import type { Response } from '@playwright/test';
import { test, expect, type Page } from './fixtures';

const MOVIES: Record<string, { folder: string; name: string }> = {
  private: { folder: '2021-01-01 Zurich trip 0', name: 'IMG_0005' },
  public: { folder: '2024-12-02 Movie 98', name: 'MOV_0001' },
};
let movie: { folder: string; name: string };

/** Whether the response is for the movie (not its neighbours). */
const isMovie = (r: Response, kind: string) =>
  r.url().includes(`/image/${kind}/`) &&
  decodeURIComponent(r.url()).includes(`${movie.folder}/${movie.name}`);

// The first frame's response; the server answers a failure (e.g. no
// ffmpeg) with an error image, also with status 200.
let frame: Response;

test.beforeEach(async ({ page }, testInfo) => {
  movie = MOVIES[testInfo.project.name];
  [frame] = await Promise.all([
    page.waitForResponse((r) => isMovie(r, 'bytes')),
    page.goto(`/view/${movie.folder}/${movie.name}`),
  ]);
  await expect(page.locator('#navtext3')).toHaveText(movie.name);
  await expect(page.locator('#persistBox')).toHaveText(/This is a movie/);
});

test('shows the first frame, and says it is a movie', async ({ page }) => {
  expect(frame.status()).toBe(200);
  expect(frame.headers()['content-type']).toMatch(/^image\/jpeg/);
  await expect(page.locator('#imageCanvas')).toBeVisible();
  // The note goes away on the next picture.
  await page.keyboard.press('ArrowRight');
  await expect(page.locator('#navtext3')).not.toHaveText(movie.name);
  await expect(page.locator('#persistBox')).toHaveText('');
});

const PLAY: [string, (p: Page) => Promise<void>][] = [
  ['the p key', (p) => p.keyboard.press('p')],
  ['the space key', (p) => p.keyboard.press(' ')],
  ['the play button', (p) => p.click('#moviePlay')],
  ['a tap on the picture', (p) => p.click('#imageCanvas')],
];

for (const [how, play] of PLAY) {
  test(`plays with ${how}, then pauses`, async ({ page }) => {
    const video = page.locator('#main video');
    const time = () =>
      video.evaluate((v) => (v as HTMLVideoElement).currentTime);
    const [resp] = await Promise.all([
      page.waitForResponse((r) => isMovie(r, 'movie')),
      play(page),
    ]);
    expect(resp.ok()).toBe(true);
    expect(resp.headers()['content-type']).toMatch(/^video\//);
    await expect(video).toBeVisible();
    await expect(page.locator('#imageCanvas')).toBeHidden();
    await expect.poll(time).toBeGreaterThan(0);
    await expect(page.locator('#moviePlay>span')).toHaveClass(/fa-pause/);
    // The movie is long enough (see add-movie.sh) not to end before this.
    await page.keyboard.press('p');
    await expect(page.locator('#moviePlay>span')).toHaveClass(/fa-play/);
    await expect
      .poll(() => video.evaluate((v) => (v as HTMLVideoElement).paused))
      .toBe(true);
    const at = await time();
    expect(at).toBeLessThan(10);
    await expect.poll(time).toBe(at);
  });
}

test('the seek bar moves the movie', async ({ page }) => {
  const video = page.locator('#main video');
  const time = () => video.evaluate((v) => (v as HTMLVideoElement).currentTime);
  await page.keyboard.press('p');
  await expect.poll(time).toBeGreaterThan(0);
  await page.keyboard.press('p');
  await expect(page.locator('#moviePlay>span')).toHaveClass(/fa-play/);
  // Half of the 10 s movie (see add-movie.sh).
  await page.locator('#seekBar').fill('50');
  await expect.poll(time).toBeCloseTo(5, 0);
});

test('a movie that fails to load says so', async ({ page, consoleErrors }) => {
  consoleErrors.allow(/status of 404/);
  consoleErrors.allow(/Failed to load movie/);
  await page.route('**/image/movie/**', (r) => r.fulfill({ status: 404 }));
  await page.keyboard.press('p');
  await expect(page.locator('#persistBox')).toHaveText(/could not be loaded/);
  await expect(page.locator('#moviePlay>span')).toHaveClass(/fa-play/);
});
