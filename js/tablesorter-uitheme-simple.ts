/* Simplification of bootstrap-theme in widget-uitheme.js */
(function ($) {
  'use strict';
  const ts = $.tablesorter || {};

  $.extend(ts.themes.bootstrap, {
    table: 'table',
    // add 'icon-white' to make them white; this icon class is added
    // to the <i> in the header
    icons: '',
    // class name added to icon when column is not sorted
    iconSortNone: 'fas fa-sort',
    // class name added to icon when column has ascending sort
    iconSortAsc: 'fas fa-sort-up',
    // class name added to icon when column has descending sort
    iconSortDesc: 'fas fa-sort-down',
  });
})(jQuery);
