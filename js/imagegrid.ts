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

// The image grid's entry point: sets up the grid (js/grid.ts) with its
// libraries, once the page is ready.

import Masonry from 'masonry-layout';
import InfiniteScroll from 'infinite-scroll';
import imagesLoaded from 'imagesloaded';
import { setupImageGrid } from './grid';
import { whenReady } from './ready';

whenReady(() =>
  setupImageGrid({
    masonry: (element, options) => new Masonry(element, options),
    imagesLoaded: (element) => imagesLoaded(element),
    infiniteScroll: (element, options) => {
      // make imagesLoaded available for InfiniteScroll
      // TODO: update the upstream type definitions, when I have time.
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (InfiniteScroll as any).imagesLoaded = imagesLoaded;
      return new InfiniteScroll(element, options);
    },
  }),
);
