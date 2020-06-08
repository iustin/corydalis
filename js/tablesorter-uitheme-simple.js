/* Simplification of bootstrap-theme in widget-uitheme.js */
;(function($) {
  'use strict';
  const ts = $.tablesorter || {};

  $.extend(ts.themes.bootstrap, {
    table: 'table',
    icons: '', // add 'icon-white' to make them white; this icon class is added to the <i> in the header
    iconSortNone: 'fas fa-sort', // class name added to icon when column is not sorted
    iconSortAsc: 'fas fa-sort-up', // class name added to icon when column has ascending sort
    iconSortDesc: 'fas fa-sort-down', // class name added to icon when column has descending sort
  });
})(jQuery);
