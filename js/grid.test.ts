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
import { describe, it } from 'node:test';

import { JSDOM } from 'jsdom';
import type InfiniteScroll from 'infinite-scroll';
import type Masonry from 'masonry-layout';

import {
  type GridLibs,
  gridSettings,
  pagePathTemplate,
  setupImageGrid,
} from './grid.ts';

/** A page with a boot element with the given attributes, and the body. */
function page(bootAttrs: string | null, body = '<div class="grid"></div>') {
  const boot = bootAttrs === null ? '' : `<div id="boot" ${bootAttrs}></div>`;
  return new JSDOM(`${boot}${body}`).window.document;
}

type Handler = (...args: never[]) => void;

/** Fake libraries, recording their use. */
function fakeLibs() {
  const calls = {
    masonry: [] as { element: Element; options: Masonry.Options }[],
    layouts: 0,
    progress: [] as (() => void)[],
    scroll: [] as { element: Element; options: InfiniteScroll.Options }[],
    handlers: {} as Record<string, Handler>,
  };
  const scroll = { pageIndex: 1 } as Pick<InfiniteScroll, 'on' | 'pageIndex'>;
  scroll.on = ((event: string, handler: Handler) => {
    calls.handlers[event] = handler;
  }) as typeof scroll.on;
  const masonry = {
    layout: () => {
      calls.layouts++;
    },
  } as unknown as Masonry;
  const libs: GridLibs = {
    masonry: (element, options) => {
      calls.masonry.push({ element, options });
      return masonry;
    },
    imagesLoaded: () => ({
      on: (_event, listener) => calls.progress.push(listener),
    }),
    infiniteScroll: (element, options) => {
      calls.scroll.push({ element, options });
      return scroll;
    },
  };
  return { libs, calls, scroll, masonry };
}

describe('gridSettings', () => {
  const boot = (attrs: string) =>
    page(attrs).getElementById('boot') as HTMLElement;

  it('reads the boot data', () => {
    assert.deepEqual(
      gridSettings(
        boot(
          'data-path-url="/browse/0?q=x" data-page-index="2" ' +
            'data-initial-count="30" data-debug="true"',
        ),
      ),
      {
        pathUrl: '/browse/0?q=x',
        pageIndex: 2,
        initialCount: 30,
        debug: true,
      },
    );
  });

  it('has defaults', () => {
    assert.deepEqual(gridSettings(boot('')), {
      pathUrl: '',
      pageIndex: 0,
      initialCount: 0,
      debug: false,
    });
  });
});

describe('pagePathTemplate', () => {
  it('replaces the first page number', () => {
    assert.equal(pagePathTemplate('/browse/0?q=x'), '/browse/{{#}}?q=x');
  });
  it('keeps URLs without one', () => {
    assert.equal(pagePathTemplate('/browse/1?q=x'), '/browse/1?q=x');
    assert.equal(pagePathTemplate(''), '');
  });
});

describe('setupImageGrid', () => {
  const BOOT =
    'data-path-url="/browse/0?q=x" data-page-index="3" ' +
    'data-initial-count="10" data-debug="false"';

  it('does nothing without a boot element or grid', () => {
    for (const doc of [page(null), page(BOOT, '')]) {
      const { libs, calls } = fakeLibs();
      setupImageGrid(libs, doc);
      assert.deepEqual(calls.masonry, []);
      assert.deepEqual(calls.scroll, []);
    }
  });

  it('lays out the grid, again as images load', () => {
    const doc = page(BOOT);
    const { libs, calls } = fakeLibs();
    setupImageGrid(libs, doc);
    const grid = doc.querySelector('.grid');
    assert.equal(calls.masonry.length, 1);
    assert.equal(calls.masonry[0].element, grid);
    assert.equal(calls.masonry[0].options.itemSelector, '.grid-item');
    assert.equal(calls.progress.length, 1);
    calls.progress[0]();
    calls.progress[0]();
    assert.equal(calls.layouts, 2);
  });

  it('loads further pages', () => {
    const doc = page(BOOT);
    const { libs, calls, scroll, masonry } = fakeLibs();
    setupImageGrid(libs, doc);
    assert.equal(calls.scroll.length, 1);
    const { element, options } = calls.scroll[0];
    assert.equal(element, doc.querySelector('.grid'));
    assert.equal(options.path, '/browse/{{#}}?q=x');
    assert.equal(options.append, '.grid-item');
    assert.equal(options.outlayer, masonry);
    assert.equal(options.debug, false);
    assert.equal(options.history, 'replace');
    assert.equal(options.checkLastPage, '.pagination-next');
    assert.equal(options.hideNav, '.pagination');
    assert.equal(options.status, '.page-load-status');
    assert.equal(scroll.pageIndex, 3);
  });

  it('counts the loaded images', (t) => {
    const log = t.mock.method(console, 'log', () => {}).mock;
    const doc = page(
      BOOT,
      '<div class="grid"></div><span id="howmany"></span>',
    );
    const { libs, calls } = fakeLibs();
    setupImageGrid(libs, doc);
    const append = calls.handlers.append as (
      response: unknown,
      path: string,
      items: { length: number },
    ) => void;
    append(null, '/browse/1', { length: 5 });
    assert.equal(doc.getElementById('howmany')?.textContent, '15');
    append(null, '/browse/2', { length: 2 });
    assert.equal(doc.getElementById('howmany')?.textContent, '17');
    assert.equal(log.callCount(), 2);
  });

  it('counts without a count element', (t) => {
    const log = t.mock.method(console, 'log', () => {}).mock;
    const { libs, calls } = fakeLibs();
    setupImageGrid(libs, page(BOOT));
    const append = calls.handlers.append as (
      response: unknown,
      path: string,
      items: { length: number },
    ) => void;
    append(null, '/browse/1', { length: 5 });
    assert.deepEqual(log.calls[0].arguments, ['now viewing', 15]);
  });

  it('logs failed page loads', (t) => {
    const errors = t.mock.method(console, 'error', () => {}).mock;
    const { libs, calls } = fakeLibs();
    setupImageGrid(libs, page(BOOT));
    const error = calls.handlers.error as (err: string, path: string) => void;
    error('boom', '/browse/1');
    assert.equal(errors.callCount(), 1);
    assert.ok((errors.calls[0].arguments as unknown[]).includes('/browse/1'));
  });
});
