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

// The image grid of the browse page: a Masonry layout, loading further
// pages on scrolling. The libraries (Masonry, imagesLoaded and Infinite
// Scroll) are passed in by js/imagegrid.ts, so that the tests can run
// this without a browser.

import type InfiniteScroll from 'infinite-scroll';
import type Masonry from 'masonry-layout';

/** The libraries used by the grid. */
export type GridLibs = {
  /** Creates the Masonry layout. */
  masonry: (element: Element, options: Masonry.Options) => Masonry;
  /** Tracks the loading of the images in the element. */
  imagesLoaded: (element: Element) => {
    on(event: 'progress', listener: () => void): unknown;
  };
  /** Creates the infinite scrolling. */
  infiniteScroll: (
    element: Element,
    options: InfiniteScroll.Options,
  ) => Pick<InfiniteScroll, 'on' | 'pageIndex'>;
};

/** The grid settings, from the boot element. */
export type GridSettings = {
  /** The URL of the first page. */
  pathUrl: string;
  /** The index of the current page. */
  pageIndex: number;
  /** The number of images on the page. */
  initialCount: number;
  debug: boolean;
};

/** Reads the grid settings from the boot element's data attributes. */
export function gridSettings(boot: HTMLElement): GridSettings {
  return {
    pathUrl: boot.dataset.pathUrl || '',
    pageIndex: parseInt(boot.dataset.pageIndex || '0', 10),
    initialCount: parseInt(boot.dataset.initialCount || '0', 10),
    debug: boot.dataset.debug === 'true',
  };
}

/**
 * Returns the path template of the pages, for Infinite Scroll: the URL of
 * the first page, with `{{#}}` for the page number.
 */
export function pagePathTemplate(pathUrl: string): string {
  return pathUrl.replace('/0?', '/{{#}}?');
}

/**
 * Sets up the image grid of the page, if it has one, updating the image
 * count (in the `howmany` element) as further pages are loaded.
 */
export function setupImageGrid(libs: GridLibs, doc: Document = document): void {
  const bootdiv = doc.getElementById('boot');
  if (!bootdiv) return;
  const settings = gridSettings(bootdiv);
  let count = settings.initialCount;
  const howmany = doc.getElementById('howmany');
  // init Packery
  const gridElement = doc.querySelector('.grid');
  if (!gridElement) return;

  const msnry = libs.masonry(gridElement, {
    itemSelector: '.grid-item',
    columnWidth: '.grid-sizer',
    percentPosition: true,
    horizontalOrder: false,
  });
  const imgLoad = libs.imagesLoaded(gridElement);
  // Layout Masonry after each image loads
  imgLoad.on('progress', () => {
    msnry.layout?.();
  });

  // infinite scrolling
  const infScroll = libs.infiniteScroll(gridElement, {
    // options
    // path: '.pagination-next',
    path: pagePathTemplate(settings.pathUrl),
    append: '.grid-item',
    outlayer: msnry,
    history: 'replace',
    checkLastPage: '.pagination-next',
    // prefill (disabled): when enabled, loads further pages right away
    // until the content fills the window, instead of only on scrolling.
    // Without it, if the first page doesn't fill a (very tall) window,
    // there's nothing to scroll and no further pages load, as the
    // pagination links are hidden (hideNav). Not seen in practice so
    // far. The option exists in infinite-scroll (page-load.js), but not
    // in its type definitions (@types/infinite-scroll), so enabling it
    // would need a cast, like imagesLoaded in js/imagegrid.ts.
    // prefill: true,
    hideNav: '.pagination',
    // Shows the loading, last page and error messages from the template.
    status: '.page-load-status',
    debug: settings.debug,
  });
  // Failed page loads are reported via this event (the library catches
  // the errors itself, so the loading promises never reject).
  infScroll.on('error', (error, path) => {
    console.error('Failed to load more images from', path, error);
  });
  // Handle appended items
  infScroll.on('append', (response, path, items) => {
    count = count + items.length;
    if (howmany) {
      howmany.textContent = count.toString();
    }
    console.log('now viewing', count);
  });

  infScroll.pageIndex = settings.pageIndex;
}
