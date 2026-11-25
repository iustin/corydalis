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

import { test, expect, type Page } from './fixtures';

const MOVIE = '/view/2024-12-02 Movie 98/MOV_0001';

test.beforeEach(async ({ page }) => {
  await page.goto(MOVIE);
  await expect(page.locator('#navtext3')).toHaveText('MOV_0001');
  await expect(page.locator('#waiting-bar')).toBeHidden();
});

test('shows the first frame, and says it is a movie', async ({ page }) => {
  await expect(page.locator('#persistBox')).toHaveText(
    "This is a movie. Press 'p', click or touch to play.",
  );
  await expect(page.locator('#imageCanvas')).toBeVisible();
  // The first frame is a real picture, not the error image.
  const [resp] = await Promise.all([
    page.waitForResponse((r) => r.url().includes('/image/bytes/')),
    page.reload(),
  ]);
  expect(resp.status()).toBe(200);
  expect(resp.headers()['content-type']).toMatch(/^image\/jpeg/);
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
    const [resp] = await Promise.all([
      page.waitForResponse((r) => r.url().includes('/movie/')),
      play(page),
    ]);
    expect(resp.ok()).toBe(true);
    await expect(video).toBeVisible();
    await expect(page.locator('#imageCanvas')).toBeHidden();
    await expect
      .poll(() => video.evaluate((v) => (v as HTMLVideoElement).currentTime))
      .toBeGreaterThan(0);
    await expect(page.locator('#moviePlay>span')).toHaveClass(/fa-pause/);
    await page.keyboard.press('p');
    await expect(page.locator('#moviePlay>span')).toHaveClass(/fa-play/);
    expect(await video.evaluate((v) => (v as HTMLVideoElement).paused)).toBe(
      true,
    );
  });
}
