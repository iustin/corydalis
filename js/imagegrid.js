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
  const bootdiv = $('#boot');
  const pathurl = bootdiv.data('path-url');
  const pageindex = bootdiv.data('page-index');
  let count = bootdiv.data('initial-count');
  const debug = bootdiv.data('debug');
  const $howmany = $('#howmany');
  // init Packery
  const $grid = $('.grid').masonry({
    itemSelector: '.grid-item',
    columnWidth: '.grid-sizer',
    percentPosition: true,
    horizontalOrder: false,
  });
    // layout Packery after each image loads
  $grid.imagesLoaded().progress( function() {
    $grid.masonry('layout');
    // $grid.packery();
  });
  // get Masonry instance
  const msnry = $grid.data('masonry');
  // infinite scrolling
  $grid.infiniteScroll({
    // options
    // path: '.pagination-next',
    path: pathurl.replace('/0?', '/{{#}}?'),
    append: '.grid-item',
    outlayer: msnry,
    history: 'replace',
    checkLastPage: '.pagination-next',
    prefill: true,
    onInit: function() {
      this.pageIndex = pageindex;
      this.on('append', function(response, path, items) {
        count = count + items.length;
        $howmany.text(count);
        console.log('now viewing', count);
      });
    },
    hideNav: '.pagination',
    debug: debug,
  });
});
