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

$(document).ready(function() {
  'use strict';
  $.fancybox.defaults.animationEffect = 'false';
  $.fancybox.defaults.transitionEffect = 'false';
  // Disable hashing since masonry controls the URL.
  $.fancybox.defaults.hash = false;
  // We know we act on a grid, which has infinite scroll enabled.
  const $grid = $('.grid');

  // Custom after show to scroll the background item into view, in
  // order to trigger infinite scroll to load more images.
  $.fancybox.defaults.afterShow = function(instance, current) {
    if (!current) {
      return;
    }
    if (current.index >= instance.group.length - 10) {
      $grid.infiniteScroll('loadNextPage');
    }
    // Object containing references to interface elements
    // (background, buttons, caption, etc)
    // console.info( instance.$refs );

    // Current slide options
    // console.info( current.opts );

    // Clicked element
    // console.info( current.opts.$orig );

    // Reference to DOM element of the slide
    // console.info( current.$slide );
  };

  // Custom view item button.
  $.fancybox.defaults.btnTpl.view =
        '<button data-fancybox-view class="fancybox-button fancybox-button--view" title="Open image in viewer">' +
        '<span class="fas fa-external-link-alt">' +
        '</button>';

  $('body').on('click', '[data-fancybox-view]', function() {
    const instance = $.fancybox.getInstance();
    const current = instance.current || null;
    if (!current) {
      return;
    }
    if (!current.opts.viewurl) {
      $.fancybox.open('<div class="message"><h2>Internal Error!</h2>' +
                            '<p>Viewed image does not have a <i>viewurl</i> data attribute!</p></div>');
      console.log('No viewurl, returning', current);
      return;
    }

    window.open(current.opts.viewurl, '');
  });

  // Custom view item info button.
  $.fancybox.defaults.btnTpl.info =
        '<button data-fancybox-info class="fancybox-button fancybox-button--info" title="View image info">' +
        '<span class="fas fa-info-circle">' +
        '</button>';

  $('body').on('click', '[data-fancybox-info]', function() {
    const instance = $.fancybox.getInstance();
    const current = instance.current || null;
    if (!current) {
      console.log('No current, returning');
      return;
    }
    if (!current.opts.infourl) {
      $.fancybox.open('<div class="message"><h2>Internal Error!</h2>' +
                            '<p>Viewed image does not have a <i>infourl</i> data attribute!</p></div>');
      console.log('No infourl, returning', current);
      return;
    }
    window.open(current.opts.infourl, '');
  });

  $.fancybox.defaults.buttons = [
    'view',
    'info',
    'zoom',
    'close',
  ];

  // Listen to grid expansion and update any in-progress slideshow.
  $('.grid').on( 'append.infiniteScroll', function( event, response, path, items ) {
    // console.log( 'Loaded: ', path );
    // console.info(items);
    const instance = $.fancybox.getInstance();
    const current = instance.current || null;
    if (!current) {
      return;
    }
    const imgs = Array.from(items, (d) => d.firstChild);
    // console.log( 'Adding items: ', imgs);

    instance.addContent(imgs);
  });

  // Trigger fancybox.
  $().fancybox({
    selector: 'div#fbox-container a.fbox-item',
    // TODO: to allow preload=false, need to pass height/width.
    preload: true,
  });
});
