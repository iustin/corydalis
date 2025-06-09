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

import { Fancybox } from '@fancyapps/ui';
import 'imagegrid';

export function initFancybox() {
  // We know we act on a grid, which has infinite scroll enabled.
  const grid = document.querySelector('.grid');
  if (!grid) return;

  // Get the infinitescroll instance

  // Handle view button click
  const viewHandler = () => {
    const instance = Fancybox.getInstance();
    const slide = instance?.getSlide();

    if (!slide) return;
    const viewurl = slide.viewurl || slide.triggerEl?.dataset.viewurl;

    if (!viewurl) {
      Fancybox.open(`<div class="message"><h2>Internal Error!</h2>
        <p>Viewed image does not have a <i>viewurl</i> data attribute!</p></div>`);
      console.log('No viewurl, returning', slide);
      return;
    }

    window.open(viewurl, '');
  };

  // Handle info button click
  const infoHandler = () => {
    const instance = Fancybox.getInstance();
    const slide = instance?.getSlide();

    if (!slide) {
      console.log('No current, returning');
      return;
    }
    const infourl = slide.infourl || slide.triggerEl?.dataset.infourl;

    if (!infourl) {
      Fancybox.open(`<div class="message"><h2>Internal Error!</h2>
        <p>Viewed image does not have a <i>infourl</i> data attribute!</p></div>`);
      console.log('No infourl, returning', slide);
      return;
    }

    window.open(infourl, '');
  };
  const options = {
    // v4 uses different animation options
    showClass: false,
    hideClass: false,
    Hash: false,
    groupAll: true,
    on: {
      reveal: (fancybox, slide) => {
        if (!slide) return;
        const threshold = fancybox.carousel.slides.length - 10;
        if (slide.index >= threshold) {
          console.log(
            'Requesting next page, index %d, threshold %d',
            slide.index,
            threshold,
          );
          grid.dispatchEvent(new CustomEvent('corydalis-load-next-page'));
        }
      },
    },
    // TODO: to allow preload=false, need to pass height/width.
    preload: true,
    // Toolbar configuration is different in v4
    Toolbar: {
      display: {
        left: ['view', 'info'],
        right: ['zoom', 'close'],
      },
      items: {
        view: {
          tpl: '<button data-fancybox-view class="f-button">View</button>',
          click: viewHandler,
        },
        info: {
          tpl: '<button data-fancybox-info class="f-button">Info</button>',
          click: infoHandler,
        },
      },
    },
  };

  // Listen to grid expansion and update any in-progress slideshow.
  if (grid) {
    // Create a custom event handler for the append event from InfiniteScroll
    const handleAppend = (event) => {
      const { items } = event.detail;
      if (!items) return;
      const instance = Fancybox.getInstance();
      if (!instance) return;

      const imgs = Array.from(items, (d) => d.firstChild);
      // console.log( 'Adding items: ', imgs);
      instance.carousel.appendSlide(imgs);
    };

    grid.addEventListener('corydalis-images-appended', handleAppend);
  }

  // Trigger fancybox.
  Fancybox.bind('div#fbox-container a.fbox-item', options);
}

// Initialize on DOMContentLoaded
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initFancybox);
} else {
  // Document already loaded, run the function directly
  initFancybox();
}
