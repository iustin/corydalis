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

import assert from 'node:assert/strict';
import { afterEach, describe, it, type TestContext } from 'node:test';

import { JSDOM, VirtualConsole } from 'jsdom';

import {
  type FullScreenApi,
  type ViewerWindow,
  downloadFile,
  relocateHelpDiv,
  startViewer,
} from './viewer-app.ts';
import {
  type ImageInfo,
  MAX_ZOOM,
  MOVIE_MESSAGE,
  type ViewInfo,
} from './viewer-logic.ts';

const BASE = 'https://cory.example';

/** The info of an image, with all URLs under BASE. */
function image(name: string, extra: Partial<ImageInfo> = {}): ImageInfo {
  const url = (kind: string) => `${BASE}/${kind}/${name}`;
  return {
    info: url('info'),
    bytes: url('bytes'),
    view: url('view'),
    flag: url('flag'),
    list: url('list'),
    browse: url('browse'),
    name,
    transform: [0, false, false],
    matrix: [1, 0, 0, 1],
    fullres: false,
    exifhtml: `<b>${name}</b>`,
    ...extra,
  };
}

/** The view info for the given current image and neighbours. */
function viewInfo(
  current: ImageInfo,
  around: Partial<
    Pick<ViewInfo, 'prev' | 'next' | 'prevfolder' | 'nextfolder'>
  > = {},
): ViewInfo {
  return {
    year: `year-${current.name}`,
    yearurl: `${BASE}/year`,
    folder: `folder-${current.name}`,
    folderurl: `${BASE}/folder`,
    folderlist: `${BASE}/folderlist`,
    image: `image-${current.name}`,
    imageurl: `${BASE}/image/${current.name}`,
    first: image('first'),
    current,
    last: image('last'),
    ...around,
  };
}

/** The viewer's page, as rendered by the server (templates/view.hamlet). */
const PAGE = `
  <nav id="nav">
    <a id="navlink1"><span id="navtext1"></span></a>
    <a id="navlink2"><span id="navtext2"></span></a>
    <a id="navlink3"><span id="navtext3"></span></a>
    <button id="menuToggle"></button>
    <div id="imageNavGroup"></div>
    <input id="entry">
  </nav>
  <div id="main" class="container-fluid">
    <canvas id="imageCanvas"></canvas>
    <div id="messageBox"></div>
    <div id="persistBox"></div>
    <div id="waiting-bar"></div>
    <div id="imageInfo"><div id="imageInfoText"></div></div>
    <div id="helpDiv"></div>
    <button id="moviePlay"><span class="fa-play"></span></button>
    <input type="range" id="seekBar">
    <button id="imageFull"><span class="fa-maximize"></span></button>
    <button id="imageRand"></button>
    <button id="goImageInfo"></button>
    <button id="downloadImage"></button>
    <button id="goParent"></button>
    <button id="goBrowse"></button>
    <button id="goList"></button>
    <button id="goFolderList"></button>
    <button id="imageZoom"><span class="fa-magnifying-glass-plus"></span></button>
    <button id="imagePrev"></button>
    <button id="imageNext"></button>
    <button id="folderPrev"></button>
    <button id="folderNext"></button>
    <div class="nav-only-image"></div>
    <div class="nav-only-video"></div>
  </div>
  <footer></footer>
  <div id="boot"></div>`;

/** A canvas 2D context recording the calls of its methods. */
class FakeContext {
  canvas: HTMLCanvasElement;
  calls: { name: string; args: unknown[] }[] = [];
  constructor(canvas: HTMLCanvasElement) {
    this.canvas = canvas;
  }
  private record(name: string, args: unknown[]) {
    this.calls.push({ name, args });
  }
  setTransform(...args: unknown[]) {
    this.record('setTransform', args);
  }
  clearRect(...args: unknown[]) {
    this.record('clearRect', args);
  }
  transform(...args: unknown[]) {
    this.record('transform', args);
  }
  drawImage(...args: unknown[]) {
    this.record('drawImage', args);
  }
  /** The arguments of the last drawImage call. */
  lastDraw(): unknown[] {
    const draws = this.draws();
    return draws[draws.length - 1];
  }
  /** The arguments of the drawImage calls. */
  draws(): unknown[][] {
    return this.calls.filter((c) => c.name === 'drawImage').map((c) => c.args);
  }
}

/** The state of a fake media element. */
type Media = { paused: boolean; currentTime: number; duration: number };

type Route = (method: string) => Response | Promise<Response>;

/** Returns a route answering with the given JSON. */
const json =
  (value: unknown): Route =>
  () =>
    new Response(JSON.stringify(value));

type SetupOptions = {
  info?: ViewInfo | null;
  html?: string;
  routes?: Record<string, Route>;
  fullScreen?: Partial<FullScreenApi>;
  resizeObserver?: boolean;
  devicePixelRatio?: number;
  debug?: boolean;
  /** The play() result of the media elements. */
  play?: () => Promise<void>;
  /** Whether to use the viewer's default navigation. */
  defaultNavigate?: boolean;
};

/** The windows created by setup, closed after each test. */
const windows: { close(): void }[] = [];

// Closing the windows cancels their timers (e.g. hiding the messages),
// which would otherwise keep the test run going until they fire.
afterEach(() => {
  for (const win of windows.splice(0)) {
    win.close();
  }
});

/**
 * Starts the viewer in a page with fake browser parts (canvas, media,
 * requests, full screen), returning the means to drive and inspect it.
 */
function setup(options: SetupOptions = {}) {
  const info =
    options.info === undefined
      ? viewInfo(image('a'), { prev: image('p'), next: image('n') })
      : options.info;
  // Recording jsdom's "not implemented" errors (e.g. for navigation),
  // instead of printing them.
  const jsdomErrors: string[] = [];
  const virtualConsole = new VirtualConsole();
  virtualConsole.on('jsdomError', (e) => jsdomErrors.push(e.message));
  const dom = new JSDOM(options.html ?? PAGE, {
    url: info?.current.view ?? `${BASE}/view/x`,
    virtualConsole,
  });
  windows.push(dom.window);
  const win = dom.window as unknown as ViewerWindow;
  const doc = win.document;
  const boot = doc.getElementById('boot');
  if (info !== null) {
    boot?.setAttribute('data-view-info', JSON.stringify(info));
  }
  boot?.setAttribute('data-random-url', `${BASE}/random`);
  boot?.setAttribute('data-debug', JSON.stringify(options.debug ?? false));

  // The canvas: fake contexts, and a fixed display size.
  const contexts = new Map<HTMLCanvasElement, FakeContext>();
  const rect = { width: 800, height: 600 };
  const canvasProto = win.HTMLCanvasElement.prototype;
  canvasProto.getContext = function (this: HTMLCanvasElement) {
    let ctx = contexts.get(this);
    if (ctx === undefined) {
      ctx = new FakeContext(this);
      contexts.set(this, ctx);
    }
    return ctx;
  } as unknown as typeof canvasProto.getContext;
  canvasProto.getBoundingClientRect = () => rect as DOMRect;
  win.HTMLElement.prototype.setPointerCapture = () => {};
  Object.defineProperty(win, 'devicePixelRatio', {
    value: options.devicePixelRatio ?? 1,
    configurable: true,
  });

  // The images created by the viewer.
  const images: HTMLImageElement[] = [];
  win.Image = function () {
    const img = doc.createElement('img');
    images.push(img);
    return img;
  } as unknown as typeof win.Image;

  // Media elements: jsdom doesn't play them.
  const media = new WeakMap<HTMLMediaElement, Media>();
  const mediaState = (el: HTMLMediaElement): Media => {
    let m = media.get(el);
    if (m === undefined) {
      m = { paused: true, currentTime: 0, duration: 20 };
      media.set(el, m);
    }
    return m;
  };
  const mediaProto = win.HTMLMediaElement.prototype;
  for (const prop of ['paused', 'currentTime', 'duration'] as const) {
    Object.defineProperty(mediaProto, prop, {
      configurable: true,
      get(this: HTMLMediaElement) {
        return mediaState(this)[prop];
      },
      set(this: HTMLMediaElement, value: never) {
        mediaState(this)[prop] = value;
      },
    });
  }
  const play = options.play ?? (() => Promise.resolve());
  mediaProto.play = function (this: HTMLMediaElement) {
    mediaState(this).paused = false;
    return play();
  };
  mediaProto.pause = function (this: HTMLMediaElement) {
    mediaState(this).paused = true;
  };

  // User-visible side effects.
  const alerts: string[] = [];
  win.alert = (message?: string) => {
    alerts.push(String(message));
  };
  const navigations: string[] = [];
  const historyMoves: string[] = [];
  win.history.back = () => historyMoves.push('back');
  win.history.forward = () => historyMoves.push('forward');

  // The server.
  const requests: { method: string; url: string }[] = [];
  const routes = options.routes ?? {};
  const fetch = (url: string, init?: RequestInit) => {
    const method = init?.method ?? 'GET';
    requests.push({ method, url });
    const route = routes[url];
    return Promise.resolve(
      route === undefined
        ? new Response('', { status: 404, statusText: 'Not Found' })
        : route(method),
    );
  };

  // Browser-only libraries.
  const modal = { toggles: 0, hides: 0 };
  const fsCalls: string[] = [];
  const fullScreen: FullScreenApi = {
    isEnabled: false,
    isFullscreen: false,
    request: () => {
      fsCalls.push('request');
      return Promise.resolve();
    },
    exit: () => {
      fsCalls.push('exit');
      return Promise.resolve();
    },
    ...options.fullScreen,
  };

  const observers: ResizeObserverCallback[] = [];
  if (options.resizeObserver) {
    win.ResizeObserver = class {
      constructor(cb: ResizeObserverCallback) {
        observers.push(cb);
      }
      observe() {}
      unobserve() {}
      disconnect() {}
    } as unknown as typeof ResizeObserver;
  }

  const cory = startViewer(win, {
    helpModal: () => ({
      toggle: () => modal.toggles++,
      hide: () => modal.hides++,
    }),
    fullScreen,
    requestEnv: {
      fetch: fetch as typeof globalThis.fetch,
      pageUrl: win.location.href,
      csrfToken: () => 'token',
    },
    navigate: options.defaultNavigate
      ? undefined
      : (url) => navigations.push(url),
  });

  const byId = (id: string) => doc.getElementById(id) as HTMLElement;
  const canvas = byId('imageCanvas') as HTMLCanvasElement;
  const offCanvas = [...contexts.keys()].find((c) => c !== canvas);

  return {
    win,
    doc,
    cory,
    images,
    alerts,
    jsdomErrors,
    navigations,
    historyMoves,
    requests,
    modal,
    fsCalls,
    observers,
    rect,
    canvas,
    byId,
    /** The context of the visible canvas. */
    ctx: contexts.get(canvas) as FakeContext,
    /** The context of the off-screen canvas. */
    offCtx: contexts.get(offCanvas as HTMLCanvasElement) as FakeContext,
    media: mediaState,
    message: () => byId('messageBox').textContent,
    messageVisible: () => byId('messageBox').style.visibility === 'visible',
    persistent: () => byId('persistBox').textContent,
    waiting: () => byId('waiting-bar').style.display === 'block',
    /** The last image created with the given source (prefix). */
    imageFor(src: string): HTMLImageElement {
      const found = images.filter((i) => i.src.startsWith(src)).pop();
      assert.ok(found, `no image loading ${src}`);
      return found;
    },
    /** Finishes loading an image, with the given size. */
    load(img: HTMLImageElement, width = 1600, height = 1200) {
      img.width = width;
      img.height = height;
      img.onload?.call(img, new win.Event('load'));
    },
    /** Presses a key, returning whether the viewer handled it. */
    key(key: string, init: KeyboardEventInit = {}): boolean {
      const e = new win.KeyboardEvent('keydown', {
        key,
        cancelable: true,
        ...init,
      });
      doc.dispatchEvent(e);
      return e.defaultPrevented;
    },
    /** Sends a pointer event to the canvas. */
    pointer(
      type: string,
      id: number,
      x: number,
      y: number,
      init: PointerEventInit = {},
    ): PointerEvent {
      const e = new win.PointerEvent(type, {
        pointerId: id,
        clientX: x,
        clientY: y,
        isPrimary: id === 1,
        pointerType: 'mouse',
        cancelable: true,
        ...init,
      });
      canvas.dispatchEvent(e);
      return e;
    },
    click(id: string) {
      byId(id).click();
    },
  };
}

type Viewer = ReturnType<typeof setup>;

/** Starts the viewer, and loads the current image. */
function loaded(options: SetupOptions = {}): Viewer {
  const v = setup(options);
  v.load(v.imageFor(`${BASE}/bytes/${v.cory!.info.current.name}?`));
  return v;
}

/** Checks that the viewer says it's loading the named image. */
function assertLoading(v: Viewer, name: string) {
  assert.match(v.message() ?? '', /[Ll]oading/);
  assert.match(v.message() ?? '', new RegExp(`\\b${name}\\b`));
}

/** Lets pending promises (e.g. replies to requests) settle. */
async function settle() {
  for (let i = 0; i < 5; i++) {
    await new Promise((resolve) => setImmediate(resolve));
  }
}

/** Compares dimensions by value (-0 being 0). */
function assertDims(actual: { x: number; y: number }, x: number, y: number) {
  assert.deepEqual({ x: actual.x + 0, y: actual.y + 0 }, { x, y });
}

/** Silences console.error, returning its mock (to read its calls). */
function errors(t: TestContext) {
  return t.mock.method(console, 'error', () => {}).mock;
}

describe('relocateHelpDiv', () => {
  it('moves the help element to the end of the body', () => {
    const doc = new JSDOM('<div id="x"><div id="helpDiv"></div></div><p></p>')
      .window.document;
    const help = relocateHelpDiv(doc);
    assert.equal(help?.id, 'helpDiv');
    assert.equal(doc.body.lastElementChild, help);
  });
  it('returns null without a help element', () => {
    assert.equal(relocateHelpDiv(new JSDOM('').window.document), null);
  });
});

describe('downloadFile', () => {
  function clicks(doc: Document) {
    const got: { href: string; download: string; attached: boolean }[] = [];
    doc.defaultView!.HTMLAnchorElement.prototype.click = function (
      this: HTMLAnchorElement,
    ) {
      got.push({
        href: this.href,
        download: this.download,
        attached: this.isConnected,
      });
    };
    return got;
  }
  it('clicks a temporary download link', () => {
    const doc = new JSDOM('', { url: BASE }).window.document;
    const got = clicks(doc);
    downloadFile(doc, `${BASE}/bytes/a`, 'a.jpg');
    assert.deepEqual(got, [
      { href: `${BASE}/bytes/a`, download: 'a.jpg', attached: true },
    ]);
    assert.equal(doc.querySelector('a'), null);
  });
  it('uses a default file name', () => {
    const doc = new JSDOM('', { url: BASE }).window.document;
    const got = clicks(doc);
    downloadFile(doc, `${BASE}/bytes/a`, '');
    assert.equal(got[0].download, 'defaultFileName');
  });
});

describe('startViewer setup', () => {
  it('aborts without the image info', () => {
    const v = setup({ info: null });
    assert.equal(v.cory, null);
    assert.equal(v.alerts.length, 1);
    assert.match(v.alerts[0], /image info/);
  });

  it('aborts if the page lacks elements', () => {
    const v = setup({ html: '<div id="main"></div><div id="boot"></div>' });
    assert.equal(v.cory, null);
    assert.equal(v.alerts.length, 1);
  });

  it('prepares the page', () => {
    const v = setup();
    const main = v.byId('main');
    assert.equal(main.classList.contains('container-fluid'), false);
    assert.equal(main.style.overflow, 'hidden');
    assert.equal(v.doc.querySelector('footer')?.style.display, 'none');
    // The help element is moved out of the main element.
    assert.equal(v.doc.body.lastElementChild?.id, 'helpDiv');
    // The canvas fills its display size.
    assert.equal(v.canvas.width, 800);
    assert.equal(v.canvas.height, 600);
    assert.deepEqual(v.win.history.state, v.cory!.info.current);
  });

  it('loads the images, at the canvas resolution', () => {
    const v = setup({ devicePixelRatio: 2 });
    assert.equal(v.canvas.width, 1600);
    assert.ok(v.waiting());
    assert.equal(v.canvas.style.cursor, 'wait');
    v.imageFor(`${BASE}/bytes/a?res=1600`);
    v.imageFor(`${BASE}/bytes/p?res=1600`);
    v.imageFor(`${BASE}/bytes/n?res=1600`);
    assert.equal(v.byId('navtext1').textContent, 'year-a');
    assert.equal(v.byId('navlink2').getAttribute('href'), `${BASE}/folder`);
    assert.equal(v.byId('navtext3').textContent, 'image-a');
  });

  it('skips missing neighbours', () => {
    const v = setup({ info: viewInfo(image('a')) });
    assert.deepEqual(
      v.images.map((i) => i.src).filter((s) => s !== ''),
      [`${BASE}/bytes/a?res=800`],
    );
  });

  it('draws the current image once loaded', () => {
    const v = setup();
    v.load(v.imageFor(`${BASE}/bytes/a?`));
    // 1600×1200 fits 800×600 at half size, centred.
    const [draw] = v.ctx.draws();
    assert.deepEqual(draw.slice(1), [-400, -300, 800, 600]);
    assert.ok(
      v.ctx.calls.some(
        (c) =>
          c.name === 'transform' &&
          JSON.stringify(c.args) === '[1,0,0,1,400,300]',
      ),
    );
    assert.equal(v.byId('imageInfoText').innerHTML, '<b>a</b>');
    assert.equal(v.message(), 'a');
    assert.equal(v.waiting(), false);
    assert.equal(v.canvas.style.cursor, 'auto');
    assert.equal(v.cory!.state.scale11, 2);
    // Already at its URL.
    assert.equal(v.win.history.length, 1);
    assert.equal(v.byId('persistBox').textContent, '');
  });

  it('draws rotated images', () => {
    const v = setup({
      info: viewInfo(
        image('a', { transform: [90, true, false], matrix: [0, 1, -1, 0] }),
      ),
    });
    v.load(v.imageFor(`${BASE}/bytes/a?`));
    // Rotated, the 1600×1200 image is 1200×1600: fitting in 800×600 is
    // 3/8 of its size; the draw size is in the image's own coordinates.
    assert.deepEqual(v.ctx.lastDraw().slice(3), [600, 450]);
    const transforms = v.ctx.calls.filter((c) => c.name === 'transform');
    assert.deepEqual(
      transforms[transforms.length - 1].args,
      [0, 1, -1, 0, 400, 300],
    );
  });

  it('pre-renders the neighbours off-screen', () => {
    const v = setup();
    const next = v.imageFor(`${BASE}/bytes/n?`);
    v.load(next);
    assert.deepEqual(v.offCtx.draws(), [[next, 0, 0]]);
  });

  it('alerts on failed image loads', () => {
    const v = setup();
    const img = v.imageFor(`${BASE}/bytes/a?`);
    img.onerror?.call(img, new v.win.Event('error'));
    assert.equal(v.alerts.length, 1);
    assert.match(v.alerts[0], /load/);
    assert.equal(v.waiting(), false);
  });

  it('alerts on failed loads of other images', () => {
    const v = loaded({
      info: viewInfo(image('a'), { next: image('n'), nextfolder: image('f') }),
    });
    const fail = (img: HTMLImageElement) =>
      img.onerror?.call(img, new v.win.Event('error'));
    // The next image, still loading when shown.
    v.key('ArrowRight');
    assert.ok(v.waiting());
    fail(v.imageFor(`${BASE}/bytes/n?`));
    assert.equal(v.alerts.length, 1);
    assert.equal(v.waiting(), false);
    // Another folder's image.
    v.key(']');
    fail(v.imageFor(`${BASE}/bytes/f?`));
    assert.equal(v.alerts.length, 2);
    // The full resolution image.
    const w = loaded();
    w.key('+');
    const full = w.cory!.state.img;
    full.onerror?.call(full, new w.win.Event('error'));
    assert.equal(w.alerts.length, 1);
    assert.equal(w.waiting(), false);
  });

  it('logs in debug mode', (t) => {
    const log = t.mock.method(console, 'log', () => {}).mock;
    for (const m of ['group', 'groupEnd', 'time', 'timeEnd'] as const) {
      t.mock.method(console, m, () => {});
    }
    loaded({ debug: true });
    assert.ok(log.callCount() > 0);
  });

  it('hides messages after a while', (t) => {
    t.mock.timers.enable({ apis: ['setTimeout'] });
    const v = loaded();
    assert.ok(v.messageVisible());
    t.mock.timers.tick(1999);
    assert.ok(v.messageVisible());
    t.mock.timers.tick(1);
    assert.equal(v.messageVisible(), false);
  });

  it('replaces messages, with their timers', (t) => {
    t.mock.timers.enable({ apis: ['setTimeout'] });
    const v = loaded({ info: viewInfo(image('a')) });
    t.mock.timers.tick(1500);
    v.key('ArrowLeft');
    v.key('ArrowLeft');
    t.mock.timers.tick(1500);
    // Still shown, as the timer restarted.
    assert.ok(v.messageVisible());
    t.mock.timers.tick(500);
    assert.equal(v.messageVisible(), false);
  });
});

describe('image navigation', () => {
  const nextInfo = viewInfo(image('n'), {
    prev: image('a'),
    next: image('n2'),
  });

  it('shows a preloaded next image', async () => {
    const v = loaded({ routes: { [`${BASE}/info/n`]: json(nextInfo) } });
    const next = v.imageFor(`${BASE}/bytes/n?`);
    v.load(next);
    assert.ok(v.key('ArrowRight'));
    assert.equal(v.ctx.lastDraw()[0], next);
    assert.equal(v.win.location.href, `${BASE}/view/n`);
    assert.deepEqual(v.win.history.state, image('n'));
    assert.equal(v.message(), 'n');
    assert.deepEqual(v.requests, [{ method: 'GET', url: `${BASE}/info/n` }]);
    await settle();
    assert.equal(v.cory!.info.image, 'image-n');
    assert.equal(v.byId('navtext3').textContent, 'image-n');
    v.imageFor(`${BASE}/bytes/n2?`);
  });

  it('waits for a next image still loading', () => {
    const v = loaded();
    const draws = v.ctx.draws().length;
    v.key('ArrowRight');
    assert.equal(v.ctx.draws().length, draws);
    assert.ok(v.waiting());
    assertLoading(v, 'n');
    const next = v.imageFor(`${BASE}/bytes/n?`);
    v.load(next);
    assert.equal(v.ctx.lastDraw()[0], next);
    assert.equal(v.waiting(), false);
  });

  it('goes back to the previous image', () => {
    const v = loaded();
    v.load(v.imageFor(`${BASE}/bytes/p?`));
    v.key('ArrowLeft');
    assert.equal(v.win.location.href, `${BASE}/view/p`);
  });

  it('says when there is no next or previous image', () => {
    const v = loaded({ info: viewInfo(image('a')) });
    v.key('ArrowRight');
    assert.match(v.message() ?? '', /No next/);
    v.key('ArrowLeft');
    assert.match(v.message() ?? '', /No previous/);
    assert.deepEqual(v.requests, []);
  });

  it('shows why the image info failed to load', async (t) => {
    const mock = errors(t);
    t.mock.timers.enable({ apis: ['setTimeout'] });
    const v = loaded();
    v.load(v.imageFor(`${BASE}/bytes/n?`));
    v.key('ArrowRight');
    await settle();
    assert.equal(mock.callCount(), 1);
    assert.match(v.message() ?? '', /404/);
    assert.equal(v.waiting(), false);
    // Errors stay shown.
    t.mock.timers.tick(10000);
    assert.ok(v.messageVisible());
  });

  it('moves to other folders', async () => {
    const folderInfo = viewInfo(image('nf'));
    const v = loaded({
      info: viewInfo(image('a'), { nextfolder: image('nf') }),
      routes: { [`${BASE}/info/nf`]: json(folderInfo) },
    });
    v.key(']');
    assertLoading(v, 'nf');
    assert.ok(v.waiting());
    v.load(v.imageFor(`${BASE}/bytes/nf?`));
    assert.equal(v.win.location.href, `${BASE}/view/nf`);
    await settle();
    assert.equal(v.cory!.info.image, 'image-nf');
    v.key('PageUp');
    assert.match(v.message() ?? '', /No previous folder/);
    v.click('folderPrev');
    assert.match(v.message() ?? '', /No previous folder/);
    v.click('folderNext');
    assert.match(v.message() ?? '', /No next folder/);
  });

  it('goes to the first and last images', () => {
    const v = loaded();
    v.key('Home');
    v.load(v.imageFor(`${BASE}/bytes/first?`));
    assert.equal(v.win.location.href, `${BASE}/view/first`);
    v.key('End');
    v.load(v.imageFor(`${BASE}/bytes/last?`));
    assert.equal(v.win.location.href, `${BASE}/view/last`);
  });

  it('goes to a random image', async () => {
    const random = viewInfo(image('r'));
    const v = loaded({
      routes: {
        [`${BASE}/random`]: json(random),
        [`${BASE}/info/r`]: json(random),
      },
    });
    v.key('r');
    assert.ok(v.waiting());
    await settle();
    assertLoading(v, 'r');
    v.load(v.imageFor(`${BASE}/bytes/r?`));
    assert.equal(v.win.location.href, `${BASE}/view/r`);
    v.click('imageRand');
    v.key('Tab');
    assert.equal(
      v.requests.filter((r) => r.url === `${BASE}/random`).length,
      3,
    );
  });

  it('says why a random image failed', async () => {
    const v = loaded();
    v.key('r');
    await settle();
    assert.match(v.message() ?? '', /random/);
    assert.match(v.message() ?? '', /404/);
    assert.equal(v.waiting(), false);
  });

  it('follows the browser history', () => {
    const v = loaded();
    v.win.dispatchEvent(
      new v.win.PopStateEvent('popstate', { state: image('h') }),
    );
    v.load(v.imageFor(`${BASE}/bytes/h?`));
    assert.equal(v.message(), 'h');
    const draws = v.ctx.draws().length;
    v.win.dispatchEvent(new v.win.PopStateEvent('popstate', { state: null }));
    assert.equal(v.ctx.draws().length, draws);
  });

  it('moves in the history with keys', () => {
    const v = loaded();
    for (const k of ['Backspace', 'Delete', '<', '>']) {
      assert.ok(v.key(k));
    }
    assert.deepEqual(v.historyMoves, ['back', 'back', 'back', 'forward']);
  });

  it('goes to other pages', () => {
    const v = loaded();
    for (const k of ['u', 'b', 'l', 'L', 'o']) {
      v.key(k);
    }
    for (const id of [
      'goParent',
      'goBrowse',
      'goList',
      'goFolderList',
      'goImageInfo',
    ]) {
      v.click(id);
    }
    const urls = [
      `${BASE}/folder`,
      `${BASE}/browse/a`,
      `${BASE}/list/a`,
      `${BASE}/folderlist`,
      `${BASE}/image/a`,
    ];
    assert.deepEqual(v.navigations, [...urls, ...urls]);
  });

  it('navigates by default by changing the location', () => {
    const v = loaded({ defaultNavigate: true });
    v.key('u');
    // jsdom doesn't navigate, but says it was asked to.
    assert.equal(v.jsdomErrors.length, 1);
    assert.match(v.jsdomErrors[0], /navigation/);
  });

  it('uses the image buttons', () => {
    const v = loaded();
    v.click('imageNext');
    assertLoading(v, 'n');
    v.click('imagePrev');
    assertLoading(v, 'p');
  });

  it('downloads the image', () => {
    const v = loaded();
    const got: string[] = [];
    v.win.HTMLAnchorElement.prototype.click = function (
      this: HTMLAnchorElement,
    ) {
      got.push(`${this.href} ${this.download}`);
    };
    v.key('D');
    v.click('downloadImage');
    assert.deepEqual(got, [`${BASE}/bytes/a a`, `${BASE}/bytes/a a`]);
  });
});

describe('flagging', () => {
  it('flags and un-flags the image', async () => {
    const v = loaded({
      routes: {
        [`${BASE}/flag/a`]: (method) =>
          new Response(JSON.stringify({ text: `done ${method}` })),
      },
    });
    v.key('x');
    await settle();
    assert.equal(v.message(), 'done PUT');
    v.key('n');
    await settle();
    assert.equal(v.message(), 'done DELETE');
    assert.deepEqual(
      v.requests.map((r) => r.method),
      ['PUT', 'DELETE'],
    );
  });

  it('shows failures', async () => {
    const v = loaded();
    v.key('x');
    await settle();
    assert.match(v.message() ?? '', /404|Not Found/);
  });
});

describe('keys', () => {
  it('ignores modifier keys alone, and the meta modifier', () => {
    const v = loaded();
    for (const k of ['Shift', 'Control', 'Alt', 'Meta']) {
      assert.equal(v.key(k), false);
    }
    assert.equal(v.key('ArrowRight', { metaKey: true }), false);
    assert.equal(v.message(), 'a');
  });

  it('ignores keys typed in the search entry', () => {
    const v = loaded();
    (v.byId('entry') as HTMLInputElement).focus();
    assert.equal(v.key('ArrowRight'), false);
    assert.equal(v.message(), 'a');
  });

  it('ignores unknown keys', () => {
    const v = loaded();
    assert.equal(v.key('q'), false);
    assert.equal(v.key('q', { altKey: true }), false);
  });

  it('toggles the help, closing it with any key', () => {
    const v = loaded();
    v.key('?');
    v.key('h');
    assert.equal(v.modal.toggles, 2);
    v.byId('helpDiv').classList.add('show');
    assert.ok(v.key('ArrowRight'));
    assert.equal(v.modal.hides, 1);
    assert.equal(v.message(), 'a');
  });

  it('opens the menu', () => {
    const v = loaded();
    let clicks = 0;
    v.byId('menuToggle').addEventListener('click', () => clicks++);
    v.key('m');
    v.key('.');
    assert.equal(clicks, 2);
  });

  it('reveals the navigation group with the menu', () => {
    const v = loaded();
    const group = v.byId('imageNavGroup');
    v.byId('menuToggle').dispatchEvent(new v.win.Event('shown.bs.dropdown'));
    assert.ok(group.classList.contains('revealed'));
    v.byId('menuToggle').dispatchEvent(new v.win.Event('hidden.bs.dropdown'));
    assert.equal(group.classList.contains('revealed'), false);
  });

  it('toggles the image information', () => {
    const v = loaded();
    const info = v.byId('imageInfo');
    v.key('i');
    assert.equal(info.style.display, 'block');
    v.key('i');
    assert.equal(info.style.display, 'none');
  });
});

describe('zooming', () => {
  /** Loads the full resolution image, after a zoom requested it. */
  function loadFull(v: Viewer) {
    const img = v.cory!.state.img;
    assert.equal(img.src, `${BASE}/bytes/a`);
    assert.equal(img.dataset.fullres, 'true');
    v.load(img, 3200, 2400);
  }

  it('loads the full resolution image to zoom in', () => {
    const v = loaded();
    v.key('+');
    assert.equal(v.cory!.state.scale, 1.1);
    assert.match(v.message() ?? '', /full/);
    assert.ok(v.waiting());
    loadFull(v);
    assert.equal(v.waiting(), false);
    // The full resolution image fits at a quarter of its size.
    const [, , , w, h] = v.ctx.lastDraw();
    assert.deepEqual([w, h], [880, 660]);
    // Then zooms without loading.
    v.key('=');
    assert.equal(v.cory!.state.img.src, `${BASE}/bytes/a`);
    assert.equal(v.ctx.lastDraw()[3], 968);
  });

  it('zooms out down to fitting the canvas', () => {
    const v = loaded();
    v.key('+');
    loadFull(v);
    v.key('-');
    assert.equal(v.cory!.state.scale, 1);
    v.key('-');
    assert.equal(v.cory!.state.scale, 1);
    assert.match(v.message() ?? '', /minimum/);
  });

  it('stops at the maximum zoom', () => {
    const v = loaded();
    v.key('+');
    loadFull(v);
    v.cory!.state.scale = MAX_ZOOM - 1;
    v.key('+');
    assert.equal(v.cory!.state.scale, MAX_ZOOM);
    assert.match(v.message() ?? '', /[Mm]aximum/);
    const draws = v.ctx.draws().length;
    v.key('+');
    assert.equal(v.ctx.draws().length, draws);
  });

  it('shows zooming out on the zoom button when zoomed in', () => {
    const v = loaded();
    const icon = () => v.byId('imageZoom').querySelector('span')!.classList;
    const shows = () =>
      ['plus', 'minus'].filter((n) =>
        icon().contains(`fa-magnifying-glass-${n}`),
      );
    assert.deepEqual(shows(), ['plus']);
    v.byId('imageZoom').click();
    // Unchanged until the full resolution image is drawn.
    assert.deepEqual(shows(), ['plus']);
    loadFull(v);
    assert.deepEqual(shows(), ['minus']);
    v.key('-');
    assert.deepEqual(shows(), ['minus']);
    v.byId('imageZoom').click();
    assert.equal(v.cory!.state.scale, 1);
    assert.deepEqual(shows(), ['plus']);
    v.key('+');
    assert.deepEqual(shows(), ['minus']);
    v.key('0');
    assert.deepEqual(shows(), ['plus']);
  });

  it('resets the zoom and panning', () => {
    const v = loaded();
    v.key('+');
    loadFull(v);
    v.cory!.state.panOffsets.x = 10;
    v.key('0');
    assert.equal(v.cory!.state.scale, 1);
    assert.equal(v.cory!.state.panOffsets.x, 0);
    const draws = v.ctx.draws().length;
    v.key('0');
    assert.equal(v.ctx.draws().length, draws);
  });

  it('zooms to pixel ratios, loading the full resolution first', () => {
    const v = loaded();
    v.key('1');
    loadFull(v);
    // 3200×2400 on 800×600: 1:1 is 4 times the fitting zoom.
    assert.equal(v.cory!.state.scale, 4);
    // Already there: nothing to redraw.
    const draws = v.ctx.draws().length;
    v.key('1');
    assert.equal(v.ctx.draws().length, draws);
    v.key('2');
    assert.equal(v.cory!.state.scale, 8);
    v.key('3');
    assert.equal(v.cory!.state.scale, 12);
    v.key('4');
    assert.equal(v.cory!.state.scale, 16);
  });

  it('toggles between fitting and 1:1', () => {
    const v = loaded();
    v.key('z');
    loadFull(v);
    assert.equal(v.cory!.state.scale, 4);
    v.key('z');
    assert.equal(v.cory!.state.scale, 1);
    // The button zooms around the centre, not the mouse position.
    v.pointer('pointermove', 1, 10, 10);
    v.click('imageZoom');
    assert.equal(v.cory!.state.scale, 4);
    assertDims(v.cory!.state.panOffsets, 0, 0);
    // While the key zooms around the mouse position.
    v.key('z');
    v.key('z');
    assertDims(v.cory!.state.panOffsets, 1170, 870);
  });

  it('pans zoomed images with alt and the arrows', () => {
    const v = loaded();
    v.key('2');
    loadFull(v);
    // Drawing replaces the offsets, so always read them from the state.
    const pan = () => v.cory!.state.panOffsets;
    v.cory!.state.panOffsets.x = 0;
    v.cory!.state.panOffsets.y = 0;
    const draws = v.ctx.draws().length;
    assert.ok(v.key('ArrowRight', { altKey: true }));
    assert.equal(pan().x, 25);
    assert.ok(v.key('ArrowDown', { altKey: true, shiftKey: true }));
    assert.equal(pan().y, 50);
    v.key('ArrowLeft', { altKey: true });
    v.key('ArrowUp', { altKey: true });
    assert.deepEqual([pan().x, pan().y], [0, 25]);
    assert.equal(v.ctx.draws().length, draws + 4);
    // Not panning beyond the image.
    pan().x = v.cory!.state.panLimits.x;
    assert.ok(v.key('ArrowRight', { altKey: true }));
    assert.equal(v.ctx.draws().length, draws + 4);
    // Not moving to the next image.
    assert.equal(v.win.location.href, `${BASE}/view/a`);
  });
});

describe('full screen', () => {
  const icon = (v: Viewer) => v.byId('imageFull').firstElementChild!.classList;

  it('uses the full screen API', () => {
    const v = loaded({ fullScreen: { isEnabled: true } });
    v.key('f');
    assert.deepEqual(v.fsCalls, ['request']);
    Object.defineProperty(v.doc, 'fullscreenElement', {
      value: v.byId('main'),
      configurable: true,
    });
    v.rect.width = 1000;
    v.doc.dispatchEvent(new v.win.Event('fullscreenchange'));
    assert.ok(icon(v).contains('fa-minimize'));
    assert.ok(v.cory!.state.fullscreen);
    // The canvas follows its new size.
    assert.equal(v.canvas.width, 1000);
  });

  it('toggles the full screen with the up arrow', () => {
    const v = loaded({ fullScreen: { isEnabled: true } });
    assert.ok(v.key('ArrowUp'));
    assert.deepEqual(v.fsCalls, ['request']);
    const w = loaded({ fullScreen: { isEnabled: true, isFullscreen: true } });
    w.cory!.state.fullscreen = true;
    assert.ok(w.key('ArrowUp'));
    assert.deepEqual(w.fsCalls, ['exit']);
  });

  it('leaves the full screen', () => {
    const v = loaded({ fullScreen: { isEnabled: true, isFullscreen: true } });
    v.cory!.state.fullscreen = true;
    v.key('f');
    assert.deepEqual(v.fsCalls, ['exit']);
    Object.defineProperty(v.doc, 'fullscreenElement', {
      value: null,
      configurable: true,
    });
    v.doc.dispatchEvent(new v.win.Event('fullscreenchange'));
    assert.ok(icon(v).contains('fa-maximize'));
    assert.equal(v.cory!.state.fullscreen, false);
  });

  it('logs failures to leave the full screen', async (t) => {
    const mock = errors(t);
    const v = loaded({
      fullScreen: {
        isEnabled: true,
        isFullscreen: true,
        exit: () => Promise.reject(new Error('no')),
      },
    });
    v.cory!.state.fullscreen = true;
    v.key('f');
    await settle();
    assert.equal(mock.callCount(), 1);
  });

  it('emulates the full screen if refused', async (t) => {
    const warn = t.mock.method(console, 'warn', () => {}).mock;
    const v = loaded({
      fullScreen: {
        isEnabled: true,
        request: () => Promise.reject(new Error('no')),
      },
    });
    v.key('f');
    await settle();
    assert.equal(warn.callCount(), 1);
    assert.ok(v.byId('nav').classList.contains('nav-hidden'));
    assert.ok(v.byId('main').classList.contains('fake-fullscreen'));
    assert.ok(icon(v).contains('fa-minimize'));
    // And leaves the emulated one.
    v.key('f');
    assert.equal(v.byId('nav').classList.contains('nav-hidden'), false);
    assert.ok(icon(v).contains('fa-maximize'));
  });

  it('emulates the full screen without the API', () => {
    const v = loaded();
    v.rect.width = 1000;
    v.click('imageFull');
    assert.ok(v.byId('main').classList.contains('fake-fullscreen'));
    assert.equal(v.canvas.width, 1000);
    v.rect.width = 800;
    v.key('f');
    assert.equal(v.byId('main').classList.contains('fake-fullscreen'), false);
    assert.equal(v.canvas.width, 800);
    assert.deepEqual(v.fsCalls, []);
  });
});

describe('resizing', () => {
  it('follows the canvas size with a resize observer', () => {
    const v = loaded({ resizeObserver: true });
    assert.equal(v.observers.length, 1);
    const draws = v.ctx.draws().length;
    v.rect.width = 400;
    const entry = (target: Element) =>
      ({ target }) as unknown as ResizeObserverEntry;
    v.observers[0]([entry(v.byId('main'))], {} as ResizeObserver);
    assert.equal(v.canvas.width, 800);
    v.observers[0](
      [entry(v.byId('main')), entry(v.canvas)],
      {} as ResizeObserver,
    );
    assert.equal(v.canvas.width, 400);
    assert.equal(v.ctx.draws().length, draws + 1);
  });

  it('follows the window size otherwise', () => {
    const v = loaded();
    const draws = v.ctx.draws().length;
    v.win.dispatchEvent(new v.win.Event('resize'));
    // Same size: nothing to do.
    assert.equal(v.ctx.draws().length, draws);
    v.rect.height = 300;
    v.win.dispatchEvent(new v.win.Event('resize'));
    assert.equal(v.canvas.height, 300);
    assert.equal(v.ctx.draws().length, draws + 1);
    // Including the pixel ratio (e.g. when moving to another screen).
    Object.defineProperty(v.win, 'devicePixelRatio', {
      value: 2,
      configurable: true,
    });
    v.win.dispatchEvent(new v.win.Event('resize'));
    assert.equal(v.canvas.height, 600);
    assert.equal(v.cory!.state.devicePixelRatio, 2);
  });

  it('assumes a size if the canvas has none', () => {
    const v = loaded();
    v.rect.width = 0;
    v.rect.height = 0;
    v.win.dispatchEvent(new v.win.Event('resize'));
    assert.deepEqual([v.canvas.width, v.canvas.height], [300, 300]);
  });
});

describe('movies', () => {
  const movieInfo = () =>
    viewInfo(image('m', { movie: `${BASE}/movie/m` }), { next: image('n') });

  /** The viewer, with a loaded movie (and its video element). */
  function movie(options: SetupOptions = {}) {
    const v = loaded({ info: movieInfo(), ...options });
    const video = v.doc.querySelector('video') as HTMLVideoElement;
    assert.ok(video);
    return { ...v, video };
  }
  const playIcon = (v: Viewer) =>
    v.byId('moviePlay').firstElementChild!.classList;

  it('prepares the movie, without loading it', () => {
    const v = setup({ info: movieInfo() });
    assert.equal(v.persistent(), MOVIE_MESSAGE);
    v.load(v.imageFor(`${BASE}/bytes/m?`));
    const video = v.doc.querySelector('video') as HTMLVideoElement;
    assert.equal(video.getAttribute('preload'), 'none');
    assert.equal(
      video.querySelector('source')?.getAttribute('src'),
      `${BASE}/movie/m`,
    );
    assert.equal(video.parentElement, v.byId('main'));
    assert.ok((v.byId('seekBar') as HTMLInputElement).disabled);
    const display = (cls: string) =>
      (v.doc.querySelector(cls) as HTMLElement).style.display;
    assert.equal(display('.nav-only-video'), 'block');
    assert.equal(display('.nav-only-image'), 'none');
  });

  it('plays and pauses', () => {
    const v = movie();
    assert.ok(v.key('p'));
    assert.equal(v.media(v.video).paused, false);
    assert.ok(playIcon(v).contains('fa-pause'));
    assert.equal(v.persistent(), '');
    v.click('moviePlay');
    assert.equal(v.media(v.video).paused, true);
    assert.ok(playIcon(v).contains('fa-play'));
  });

  it('shows the movie hint again on redraws', () => {
    const v = movie();
    v.key('p');
    assert.equal(v.persistent(), '');
    v.rect.width = 400;
    v.win.dispatchEvent(new v.win.Event('resize'));
    assert.equal(v.persistent(), MOVIE_MESSAGE);
  });

  it('shows the movie once it can play', () => {
    const v = movie();
    v.key('p');
    v.video.dispatchEvent(new v.win.Event('loadeddata'));
    assert.equal(v.video.style.visibility, 'visible');
    assert.equal(v.canvas.style.visibility, 'hidden');
    assert.equal((v.byId('seekBar') as HTMLInputElement).disabled, false);
  });

  it('follows and seeks the movie with the seek bar', () => {
    const v = movie();
    const seekBar = v.byId('seekBar') as HTMLInputElement;
    v.media(v.video).currentTime = 5;
    v.video.dispatchEvent(new v.win.Event('timeupdate'));
    assert.equal(seekBar.valueAsNumber, 25);
    seekBar.valueAsNumber = 50;
    seekBar.dispatchEvent(new v.win.Event('input'));
    assert.equal(v.media(v.video).currentTime, 10);
  });

  it('shows why the movie does not play', async (t) => {
    const mock = errors(t);
    const v = movie({
      play: () =>
        Promise.reject(new DOMException('blocked', 'NotAllowedError')),
    });
    v.key('p');
    await settle();
    assert.ok(playIcon(v).contains('fa-play'));
    assert.match(v.persistent() ?? '', /allow/);
    assert.equal(mock.callCount(), 1);
  });

  it('says nothing for interrupted plays', async (t) => {
    const mock = errors(t);
    const v = movie({
      play: () => Promise.reject(new DOMException('stop', 'AbortError')),
    });
    v.key('p');
    await settle();
    assert.ok(playIcon(v).contains('fa-play'));
    assert.equal(v.persistent(), '');
    assert.equal(mock.callCount(), 0);
  });

  it('ignores play failures after moving to another image', async () => {
    let fail: (err: unknown) => void = () => {};
    const v = movie({
      play: () =>
        new Promise((_, reject) => {
          fail = reject;
        }),
    });
    v.key('p');
    v.load(v.imageFor(`${BASE}/bytes/n?`));
    v.key('ArrowRight');
    // The video is dropped with the move, stopping its loading.
    assert.equal(v.doc.querySelector('video'), null);
    assert.equal(v.cory!.state.video, null);
    assert.equal(v.video.getAttribute('src'), '');
    assert.equal(v.media(v.video).paused, true);
    fail(new DOMException('blocked', 'NotAllowedError'));
    await settle();
    assert.ok(playIcon(v).contains('fa-pause'));
  });

  it('plays with the space key, then moves on once played', () => {
    const v = movie();
    v.key(' ');
    assert.equal(v.media(v.video).paused, false);
    v.key(' ');
    assert.equal(v.media(v.video).paused, true);
    v.media(v.video).currentTime = 20;
    v.key(' ');
    assertLoading(v, 'n');
  });

  it('plays with a tap', (t) => {
    t.mock.timers.enable({ apis: ['Date'], now: 1000 });
    const v = movie();
    v.pointer('pointerdown', 1, 100, 100);
    v.pointer('pointerup', 1, 100, 100);
    assert.equal(v.media(v.video).paused, false);
  });

  it('replaces the movie when moving to another one', () => {
    const v = loaded({
      info: viewInfo(image('m', { movie: `${BASE}/movie/m` }), {
        next: image('m2', { movie: `${BASE}/movie/m2` }),
      }),
    });
    v.load(v.imageFor(`${BASE}/bytes/m2?`));
    v.key('ArrowRight');
    const videos = v.doc.querySelectorAll('video source');
    assert.equal(videos.length, 1);
    assert.equal(videos[0].getAttribute('src'), `${BASE}/movie/m2`);
  });

  it('moves to the next image with space for images', () => {
    const v = loaded();
    v.key(' ');
    assertLoading(v, 'n');
  });

  it('plays nothing for images', () => {
    const v = loaded();
    assert.equal(v.key('p'), true);
    assert.equal(v.message(), 'a');
    assert.deepEqual(v.requests, []);
    assert.equal(v.win.location.href, `${BASE}/view/a`);
  });
});

describe('pointer gestures', () => {
  /** A viewer with mocked time, starting at 1000 ms. */
  function timed(t: TestContext, options: SetupOptions = {}) {
    t.mock.timers.enable({ apis: ['Date', 'setTimeout'], now: 1000 });
    return loaded(options);
  }

  /** A quick gesture of the primary pointer, from one point to another. */
  function swipe(
    t: TestContext,
    v: Viewer,
    from: [number, number],
    to: [number, number],
    ms = 100,
  ) {
    v.pointer('pointerdown', 1, ...from);
    t.mock.timers.tick(ms);
    v.pointer('pointerup', 1, ...to);
  }

  it('tracks the mouse position', () => {
    const v = loaded();
    v.pointer('pointermove', 1, 10, 20);
    assert.deepEqual(
      [v.cory!.state.pointerLocation.x, v.cory!.state.pointerLocation.y],
      [10, 20],
    );
    v.pointer('pointermove', 2, 30, 40);
    assert.equal(v.cory!.state.pointerLocation.x, 10);
  });

  it('moves between images with swipes', (t) => {
    const v = timed(t);
    swipe(t, v, [500, 300], [300, 300]);
    assertLoading(v, 'n');
    swipe(t, v, [300, 300], [500, 310]);
    assertLoading(v, 'p');
  });

  it('ignores vertical swipes, slow gestures and swipes when zoomed', (t) => {
    const v = timed(t);
    swipe(t, v, [300, 100], [300, 400]);
    swipe(t, v, [500, 300], [300, 300], 500);
    v.cory!.state.scale = 2;
    swipe(t, v, [500, 300], [300, 300]);
    assert.equal(v.message(), 'a');
  });

  it('zooms with double taps, around the tap', (t) => {
    const v = timed(t);
    swipe(t, v, [100, 100], [100, 100], 10);
    t.mock.timers.tick(10);
    swipe(t, v, [100, 100], [100, 100], 10);
    assert.equal(v.cory!.state.img.dataset.fullres, 'true');
    v.load(v.cory!.state.img, 3200, 2400);
    assert.equal(v.cory!.state.scale, 4);
    // The tapped point stays in place: (100, 100) is (-300, -200) from
    // the centre, moved 3 times further out by the 4 times zoom.
    assertDims(v.cory!.state.panOffsets, 900, 600);
    // A third tap, even shortly after the first, is a single one.
    t.mock.timers.tick(10);
    swipe(t, v, [100, 100], [100, 100], 10);
    assert.equal(v.cory!.state.scale, 4);
  });

  it('forgets cancelled pointers', (t) => {
    const v = timed(t);
    v.pointer('pointerdown', 1, 500, 300);
    v.pointer('pointercancel', 1, 500, 300);
    v.pointer('pointerup', 1, 300, 300);
    assert.equal(v.message(), 'a');
    // Cancelling another pointer keeps the gesture.
    v.pointer('pointerdown', 1, 500, 300);
    v.pointer('pointercancel', 2, 500, 300);
    v.pointer('pointerup', 1, 300, 300);
    assertLoading(v, 'n');
  });

  it('prevents the browser touch handling', () => {
    const v = loaded();
    const e = v.pointer('pointerdown', 1, 10, 10, { pointerType: 'touch' });
    assert.ok(e.defaultPrevented);
    const m = v.pointer('pointerdown', 1, 10, 10);
    assert.equal(m.defaultPrevented, false);
    for (const name of ['gesturestart', 'gesturechange', 'gestureend']) {
      const g = new v.win.Event(name, { cancelable: true });
      v.doc.dispatchEvent(g);
      assert.ok(g.defaultPrevented, name);
    }
  });

  it('pans zoomed images by dragging', () => {
    const v = loaded();
    v.key('2');
    v.load(v.cory!.state.img, 3200, 2400);
    // Drawing replaces the offsets, so always read them from the state.
    const pan = () => v.cory!.state.panOffsets;
    v.cory!.state.panOffsets.x = 0;
    v.cory!.state.panOffsets.y = 0;
    const draws = v.ctx.draws().length;
    v.pointer('pointerdown', 1, 100, 100);
    // Too small a move.
    v.pointer('pointermove', 1, 100.5, 100);
    assert.equal(v.ctx.draws().length, draws);
    v.pointer('pointermove', 1, 150, 120);
    assert.deepEqual([pan().x, pan().y], [50, 20]);
    assert.equal(v.ctx.draws().length, draws + 1);
    // At the limits, nothing to redraw.
    pan().x = v.cory!.state.panLimits.x;
    pan().y = v.cory!.state.panLimits.y;
    v.pointer('pointermove', 1, 200, 200);
    assert.equal(v.ctx.draws().length, draws + 1);
  });

  it('zooms by pinching', () => {
    const v = loaded();
    v.key('+');
    v.load(v.cory!.state.img, 3200, 2400);
    const scale = v.cory!.state.scale;
    v.pointer('pointerdown', 1, 100, 100);
    v.pointer('pointerdown', 2, 300, 100);
    const e = v.pointer('pointermove', 2, 500, 100, { pointerType: 'touch' });
    assert.ok(e.defaultPrevented);
    // Twice the distance between the fingers.
    assert.ok(Math.abs(v.cory!.state.scale - 2 * scale) < 1e-9);
    // Around the centre between them, at (300, 100).
    assertDims(v.cory!.state.panOffsets, 180, 260);
    // Pinching further zooms relative to the last move.
    v.pointer('pointermove', 2, 700, 100);
    assert.ok(Math.abs(v.cory!.state.scale - 3 * scale) < 1e-9);
  });

  it('pans by moving two fingers', () => {
    const v = loaded();
    v.key('2');
    v.load(v.cory!.state.img, 3200, 2400);
    // Drawing replaces the offsets, so always read them from the state.
    const pan = () => v.cory!.state.panOffsets;
    v.cory!.state.panOffsets.x = 0;
    v.cory!.state.panOffsets.y = 0;
    const draws = v.ctx.draws().length;
    v.pointer('pointerdown', 1, 100, 100);
    v.pointer('pointerdown', 2, 300, 100);
    // The centre moves by 2 pixels; the distance barely changes.
    const e = v.pointer('pointermove', 2, 300, 104);
    assert.equal(e.defaultPrevented, false);
    assert.deepEqual([pan().x, pan().y], [0, 2]);
    assert.equal(v.ctx.draws().length, draws + 1);
    // Small centre moves are ignored.
    v.pointer('pointermove', 2, 301, 104);
    assert.equal(v.ctx.draws().length, draws + 1);
    // As are pans beyond the limits.
    pan().y = v.cory!.state.panLimits.y;
    v.pointer('pointermove', 2, 300, 110);
    assert.equal(v.ctx.draws().length, draws + 1);
  });

  it('ignores moves of a third finger', () => {
    const v = loaded();
    v.pointer('pointerdown', 1, 100, 100);
    v.pointer('pointerdown', 2, 300, 100);
    v.pointer('pointerdown', 3, 200, 300);
    v.pointer('pointermove', 3, 250, 350);
    assert.equal(v.cory!.state.scale, 1);
  });

  it('zooms with the mouse wheel', () => {
    const v = loaded();
    const wheel = (deltaX: number, deltaY: number) => {
      const e = new v.win.WheelEvent('wheel', {
        deltaX,
        deltaY,
        clientX: 10,
        clientY: 10,
        cancelable: true,
      });
      v.canvas.dispatchEvent(e);
      return e;
    };
    assert.ok(wheel(10, 0).defaultPrevented);
    assert.equal(v.cory!.state.scale, 1);
    wheel(0, 10);
    assert.equal(v.cory!.state.scale, 1.1);
    v.load(v.cory!.state.img, 3200, 2400);
    // Around the wheel's position, (10, 10): (-390, -290) from the
    // centre, moving out by a tenth.
    assertDims(v.cory!.state.panOffsets, 39, 29);
    wheel(0, -10);
    assert.ok(Math.abs(v.cory!.state.scale - 1) < 1e-9);
  });
});
