/* Simplification of bootstrap-theme in widget-uitheme.js */
;(function ($) {
    'use strict';
    var ts = $.tablesorter || {};

    $.extend(ts.themes.bootstrap, {
        table        : 'table table-bordered',
        icons        : '', // add 'icon-white' to make them white; this icon class is added to the <i> in the header
        iconSortNone : 'fa fa-sort', // class name added to icon when column is not sorted
        iconSortAsc  : 'fa fa-sort-asc', // class name added to icon when column has ascending sort
        iconSortDesc : 'fa fa-sort-desc', // class name added to icon when column has descending sort
    });


})(jQuery);
