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
import Masonry from 'masonry-layout';
import InfiniteScroll from 'infinite-scroll';
import imagesLoaded from 'imagesloaded';

function initImageGrid(): void {
  const bootdiv = document.getElementById('boot');
  if (!bootdiv) return;
  const pathurl = bootdiv.dataset.pathUrl || '';
  const pageindex = parseInt(bootdiv.dataset.pageIndex || '0', 10);
  let count = parseInt(bootdiv.dataset.initialCount || '0', 10);
  const debug = bootdiv.dataset.debug === 'true';
  const howmany = document.getElementById('howmany');
  // init Packery
  const gridElement = document.querySelector('.grid');
  if (!gridElement) return;

  const msnry: Masonry = new Masonry(gridElement, {
    itemSelector: '.grid-item',
    columnWidth: '.grid-sizer',
    percentPosition: true,
    horizontalOrder: false,
  });
  const imgLoad = imagesLoaded(gridElement);
  // Layout Masonry after each image loads
  imgLoad.on('progress', () => {
    msnry.layout?.();
  });

  // make imagesLoaded available for InfiniteScroll
  // TODO: update the upstream type definitions, when I have time.
  (InfiniteScroll as any).imagesLoaded = imagesLoaded;

  // infinite scrolling
  const infScroll = new InfiniteScroll(gridElement, {
    // options
    // path: '.pagination-next',
    path: pathurl.replace('/0?', '/{{#}}?'),
    append: '.grid-item',
    outlayer: msnry,
    history: 'replace',
    checkLastPage: '.pagination-next',
    // TODO: what was this about? Conversion to plain JS uncovered this doesn't exist.
    // prefill: true,
    hideNav: '.pagination',
    debug: debug,
  });
  // Handle appended items
  infScroll.on('append', (response, path, items) => {
    count = count + items.length;
    if (howmany) {
      howmany.textContent = count.toString();
    }
    console.log('now viewing', count);
    // Also trigger a custom event on the grid when items are appended.
    const appendEvent = new CustomEvent('corydalis-images-appended', {
      detail: { items },
    });
    gridElement.dispatchEvent(appendEvent);
  });
  // Listen to requests to load the next page, so that the infScroll
  // instance doesn't need to be exported.
  gridElement.addEventListener('corydalis-load-next-page', () => {
    infScroll.loadNextPage();
  });

  infScroll.pageIndex = pageindex;
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initImageGrid);
} else {
  // Document already loaded, run the function directly.
  initImageGrid();
}
