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

// Movies in the viewer: the first frame (extracted by the server with
// ffmpeg) is shown with a note, and playing switches to the video
// element, served by the server. The movie is added by add-movie.sh,
// to the public project's library.

import type { Response } from '@playwright/test';
import { test, expect, type Page } from './fixtures';

const MOVIE = '/view/2024-12-02 Movie 98/MOV_0001';

/** Whether the response is for the movie (not its neighbours). */
const isMovie = (r: Response, kind: string) =>
  r.url().includes(`/image/${kind}/`) &&
  decodeURIComponent(r.url()).includes('Movie 98/MOV_0001');

// The first frame's response; the server answers a failure (e.g. no
// ffmpeg) with an error image, also with status 200.
let frame: Response;

test.beforeEach(async ({ page }) => {
  [frame] = await Promise.all([
    page.waitForResponse((r) => isMovie(r, 'bytes')),
    page.goto(MOVIE),
  ]);
  await expect(page.locator('#navtext3')).toHaveText('MOV_0001');
  await expect(page.locator('#persistBox')).toHaveText(/This is a movie/);
});

test('shows the first frame, and says it is a movie', async ({ page }) => {
  expect(frame.status()).toBe(200);
  expect(frame.headers()['content-type']).toMatch(/^image\/jpeg/);
  await expect(page.locator('#imageCanvas')).toBeVisible();
  // The note goes away on the next picture.
  await page.keyboard.press('ArrowRight');
  await expect(page.locator('#navtext3')).not.toHaveText('MOV_0001');
  await expect(page.locator('#persistBox')).toHaveText('');
});

const PLAY: [string, (p: Page) => Promise<void>][] = [
  ['the p key', (p) => p.keyboard.press('p')],
  ['the play button', (p) => p.click('#moviePlay')],
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
