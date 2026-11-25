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

// The viewer (js/viewer-app.ts) in a real browser: boot data, canvas
// sizing, navigation (buttons, keys, swipes, history), zoom and pan
// (keys, wheel, drag, double click), the overlays, downloading,
// flagging, and the keys that leave the viewer. The logic behind these
// (zoom maths, gesture classification) is unit tested in
// js/viewer-logic.test.ts; here we check that the events reach it and
// that the result is drawn.

import { test, expect, FOLDER, FOLDER_URL, type Page } from './fixtures';

const VIEW = FOLDER_URL.replace('/folder/', '/view/');
const CANVAS = '#imageCanvas';

/** A hash of what the canvas shows. */
function canvasHash(page: Page): Promise<number> {
  return page.$eval(CANVAS, (c) => {
    const d = (c as HTMLCanvasElement).toDataURL();
    let h = 0;
    for (let i = 0; i < d.length; i++) h = (h * 31 + d.charCodeAt(i)) | 0;
    return h;
  });
}

/** Waits until the canvas shows something else than `before`. */
async function canvasChanged(page: Page, before: number): Promise<number> {
  await expect
    .poll(() => canvasHash(page), { message: 'canvas redrawn' })
    .not.toBe(before);
  return canvasHash(page);
}

/** Waits until the viewer shows the given image (URL and navigation bar). */
async function showing(page: Page, name: string, folder = FOLDER) {
  await page.waitForURL(
    (u) => decodeURIComponent(u.pathname) === `/view/${folder}/${name}`,
  );
  await expect(page.locator('#navtext3')).toHaveText(name);
}

/** The folder part of the current viewer URL. */
function currentFolder(page: Page): string {
  return decodeURIComponent(new URL(page.url()).pathname).split('/')[2];
}

/** Whether the zoom button offers to zoom out (i.e. we're zoomed in). */
function zoomedIn(page: Page) {
  return expect(page.locator('#imageZoom>span'));
}

async function canvasCentre(page: Page) {
  const box = (await page.locator(CANVAS).boundingBox())!;
  return { x: box.x + box.width / 2, y: box.y + box.height / 2 };
}

/** A quick horizontal pointer swipe over the canvas, by dx pixels. */
async function swipe(page: Page, dx: number) {
  const { x, y } = await canvasCentre(page);
  await page.mouse.move(x, y);
  await page.mouse.down();
  await page.mouse.move(x + dx, y, { steps: 3 });
  await page.mouse.up();
}

/**
 * Waits until the canvas stops changing (e.g. a redraw after the web
 * fonts change the navigation bar's height), and returns its hash.
 */
async function settled(page: Page): Promise<number> {
  let prev = await canvasHash(page);
  await expect
    .poll(
      async () => {
        await page.waitForTimeout(200);
        const h = await canvasHash(page);
        const same = h === prev;
        prev = h;
        return same;
      },
      { message: 'canvas settled' },
    )
    .toBe(true);
  return prev;
}

let fit: number;

test.beforeEach(async ({ page }) => {
  await page.goto(`${VIEW}/IMG_0002`);
  await expect(page.locator('#navtext3')).toHaveText('IMG_0002');
  await expect(page.locator('#waiting-bar')).toBeHidden();
  // The canvas starts blank; wait for the image to be drawn.
  const blank = await page.$eval(CANVAS, (c) => {
    const e = document.createElement('canvas');
    e.width = (c as HTMLCanvasElement).width;
    e.height = (c as HTMLCanvasElement).height;
    let h = 0;
    const d = e.toDataURL();
    for (let i = 0; i < d.length; i++) h = (h * 31 + d.charCodeAt(i)) | 0;
    return h;
  });
  await canvasChanged(page, blank);
  fit = await settled(page);
});

test.describe('layout', () => {
  test('boots, sized to the page', async ({ page }) => {
    const [w, h] = await page.$eval(CANVAS, (c) => [
      (c as HTMLCanvasElement).width,
      (c as HTMLCanvasElement).height,
    ]);
    expect(w).toBe(1280);
    // The navigation bar takes the rest.
    expect(h).toBeGreaterThan(800);
    expect(h).toBeLessThan(900);
    await expect(page.locator('footer')).toHaveCSS('display', 'none');
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-plus/);
  });

  test('the canvas follows the window size, and is redrawn', async ({
    page,
  }) => {
    await page.setViewportSize({ width: 800, height: 600 });
    await expect
      .poll(() => page.$eval(CANVAS, (c) => (c as HTMLCanvasElement).width))
      .toBe(800);
    await canvasChanged(page, fit);
  });
});

test.describe('navigation', () => {
  test('next and previous, with the buttons', async ({ page }) => {
    await page.click('#imageNext');
    await showing(page, 'IMG_0003');
    await expect(page.locator('#navlink3')).toHaveAttribute('href', /IMG_0003/);
    await expect(page.locator('#messageBox')).toHaveText('IMG_0003');
    await page.click('#imagePrev');
    await showing(page, 'IMG_0002');
  });

  test('next and previous, with the keyboard', async ({ page }) => {
    await page.keyboard.press('ArrowRight');
    await showing(page, 'IMG_0003');
    await page.keyboard.press('ArrowLeft');
    await showing(page, 'IMG_0002');
    // Space moves forward on images.
    await page.keyboard.press(' ');
    await showing(page, 'IMG_0003');
  });

  test('first and last image, and nothing before the first', async ({
    page,
  }) => {
    await page.keyboard.press('Home');
    await showing(page, 'IMG_0001');
    // Wait for the new neighbours before going further back.
    await expect(page.locator('#messageBox')).toHaveText('IMG_0001');
    await page.keyboard.press('ArrowLeft');
    await expect(page.locator('#messageBox')).toHaveText('No previous image');
    await showing(page, 'IMG_0001');
    await page.keyboard.press('End');
    await showing(page, 'IMG_0120', '2024-12-26 Rome trip 19');
  });

  test('next and previous folder', async ({ page }) => {
    await page.keyboard.press(']');
    await expect.poll(() => currentFolder(page)).not.toBe(FOLDER);
    const next = currentFolder(page);
    await page.keyboard.press('[');
    await expect.poll(() => currentFolder(page)).toBe(FOLDER);
    await page.keyboard.press('PageDown');
    await expect.poll(() => currentFolder(page)).toBe(next);
    await page.keyboard.press('PageUp');
    await expect.poll(() => currentFolder(page)).toBe(FOLDER);
    // FOLDER is the oldest one.
    await page.keyboard.press('PageUp');
    await expect(page.locator('#messageBox')).toHaveText(
      'No previous folder available',
    );
  });

  test('history: back and forward keys, and the browser back', async ({
    page,
  }) => {
    await page.keyboard.press('ArrowRight');
    await showing(page, 'IMG_0003');
    await page.keyboard.press('ArrowRight');
    await showing(page, 'IMG_0004');
    await page.keyboard.press('<');
    await showing(page, 'IMG_0003');
    await page.keyboard.press('>');
    await showing(page, 'IMG_0004');
    await page.goBack();
    await showing(page, 'IMG_0003');
    // The image is drawn, not only the URL changed.
    await expect(page.locator('#messageBox')).toHaveText('IMG_0003');
  });

  test('swipes move to the next and previous image', async ({ page }) => {
    await swipe(page, -300);
    await showing(page, 'IMG_0003');
    await swipe(page, 300);
    await showing(page, 'IMG_0002');
  });

  test('random image, with the button and the key', async ({ page }) => {
    for (const go of [
      () => page.click('#imageRand'),
      () => page.keyboard.press('r'),
    ]) {
      const before = page.url();
      const [resp] = await Promise.all([
        page.waitForResponse((r) => r.url().includes('/image/random')),
        go(),
      ]);
      expect(resp.ok()).toBe(true);
      await page.waitForURL(
        (u) => u.href !== before && u.pathname.startsWith('/view/'),
      );
    }
  });
});

test.describe('zoom and pan', () => {
  // The sample images (320×240) are smaller than a desktop canvas, and
  // the viewer never zooms out below fit, so there 1:1 and 2:1 would
  // be no-ops; in a small window they zoom in.
  test.use({ viewport: { width: 480, height: 240 } });

  // The first zoom loads the full resolution image (instead of the
  // one sized for the canvas), which looks slightly different even at
  // fit, so first zoom once and come back, and use that as fit.
  let full: number;
  test.beforeEach(async ({ page }) => {
    await page.keyboard.press('+');
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-minus/);
    await page.keyboard.press('0');
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-plus/);
    full = await settled(page);
  });

  test('zoom in and out with the keys, and back to fit', async ({ page }) => {
    await page.keyboard.press('+');
    const zoomed = await canvasChanged(page, full);
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-minus/);
    await page.keyboard.press('=');
    await canvasChanged(page, zoomed);
    // Not back to exactly `zoomed`: 1.1 × 1.1 / 1.1 is not 1.1.
    await page.keyboard.press('-');
    await page.keyboard.press('-');
    await expect.poll(() => canvasHash(page)).toBe(full);
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-plus/);
    await page.keyboard.press('-');
    await expect(page.locator('#messageBox')).toHaveText(
      'Already at minimum zoom',
    );
  });

  test('pixel ratios, and toggling with z and the button', async ({ page }) => {
    await page.keyboard.press('2');
    const two = await canvasChanged(page, full);
    await page.keyboard.press('3');
    await canvasChanged(page, two);
    await page.keyboard.press('0');
    await expect.poll(() => canvasHash(page)).toBe(full);
    await page.keyboard.press('z');
    await canvasChanged(page, full);
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-minus/);
    await page.keyboard.press('z');
    await expect.poll(() => canvasHash(page)).toBe(full);
    await page.click('#imageZoom');
    await canvasChanged(page, full);
    await page.click('#imageZoom');
    await expect.poll(() => canvasHash(page)).toBe(full);
  });

  test('alt+arrows pan a zoomed image', async ({ page }) => {
    // Zoom around the centre, so that there is room to pan both ways.
    const { x, y } = await canvasCentre(page);
    await page.mouse.move(x, y);
    await page.keyboard.press('3');
    const zoomed = await canvasChanged(page, full);
    await page.keyboard.press('Alt+ArrowRight');
    await canvasChanged(page, zoomed);
    await page.keyboard.press('Alt+ArrowLeft');
    await expect.poll(() => canvasHash(page)).toBe(zoomed);
    await page.keyboard.press('Alt+ArrowDown');
    await canvasChanged(page, zoomed);
  });

  test('the mouse wheel zooms', async ({ page }) => {
    const { x, y } = await canvasCentre(page);
    await page.mouse.move(x, y);
    await page.mouse.wheel(0, 100);
    const zoomed = await canvasChanged(page, full);
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-minus/);
    await page.mouse.wheel(0, -100);
    await canvasChanged(page, zoomed);
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-plus/);
  });

  test('dragging pans a zoomed image, without changing image', async ({
    page,
  }) => {
    await page.keyboard.press('3');
    const zoomed = await canvasChanged(page, full);
    const { x, y } = await canvasCentre(page);
    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x - 150, y - 50, { steps: 10 });
    await page.mouse.up();
    await canvasChanged(page, zoomed);
    await showing(page, 'IMG_0002');
  });

  test('a double click zooms in, and another back out', async ({ page }) => {
    const { x, y } = await canvasCentre(page);
    await page.mouse.dblclick(x, y);
    await canvasChanged(page, full);
    await zoomedIn(page).toHaveClass(/fa-magnifying-glass-minus/);
    await page.mouse.dblclick(x, y);
    await expect.poll(() => canvasHash(page)).toBe(full);
    await showing(page, 'IMG_0002');
  });
});

test.describe('overlays', () => {
  test('image information', async ({ page }) => {
    const info = page.locator('#imageInfo');
    await expect(info).toBeHidden();
    await page.keyboard.press('i');
    await expect(info).toBeVisible();
    await expect(info).toContainText('E-M1MarkIII');
    await page.keyboard.press('i');
    await expect(info).toBeHidden();
  });

  test('help, closed by any key', async ({ page }) => {
    const help = page.locator('#helpDiv');
    await page.keyboard.press('?');
    await expect(help).toBeVisible();
    // The closing key does nothing else.
    await page.keyboard.press('ArrowRight');
    await expect(help).toBeHidden();
    await showing(page, 'IMG_0002');
    await page.keyboard.press('h');
    await expect(help).toBeVisible();
    await page.keyboard.press('Escape');
    await expect(help).toBeHidden();
  });

  test('the menu', async ({ page }) => {
    const menu = page.locator('#imageMenu');
    await expect(menu).toBeHidden();
    await page.keyboard.press('m');
    await expect(menu).toBeVisible();
    await page.keyboard.press('.');
    await expect(menu).toBeHidden();
  });
});

test('download', async ({ page }) => {
  const [d] = await Promise.all([
    page.waitForEvent('download'),
    page.keyboard.press('D'),
  ]);
  expect(d.suggestedFilename()).toBe('IMG_0002.jpeg');
  expect(await d.failure()).toBeNull();
});

test('flag and un-flag via the keyboard', async ({ page }) => {
  const flag = (key: string) =>
    Promise.all([
      page.waitForResponse((r) => r.url().includes('/image/flag/')),
      page.keyboard.press(key),
    ]).then(([r]) => r);
  let r = await flag('x');
  expect(r.request().method()).toBe('PUT');
  expect(r.request().headers()['x-xsrf-token'], 'CSRF token sent').toBeTruthy();
  expect(r.ok()).toBe(true);
  await expect(page.locator('#messageBox')).toHaveText('Image flagged');
  r = await flag('n');
  expect(r.request().method()).toBe('DELETE');
  expect(r.ok()).toBe(true);
  await expect(page.locator('#messageBox')).toHaveText('Image flag removed');
});

test.describe('leaving the viewer', () => {
  for (const [key, path] of [
    ['u', `/folder/${FOLDER}`],
    ['o', `/folder/${FOLDER}/images/IMG_0002`],
    ['b', '/search/images/grid/0'],
    ['l', '/search/images/list'],
    ['L', '/search/folders/list'],
  ]) {
    test(`${key} goes to ${path}`, async ({ page }) => {
      await page.keyboard.press(key);
      await page.waitForURL((u) => decodeURIComponent(u.pathname) === path);
      await expect(page.locator('nav.navbar')).toBeVisible();
    });
  }
});
