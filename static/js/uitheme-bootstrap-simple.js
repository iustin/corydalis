/* Simplification of bootstrap-theme in widget-uitheme.js */
;(function ($) {
    'use strict';
    var ts = $.tablesorter || {};

    ts.themes = {
        'bootstrap-simple' : {
            table        : 'table table-bordered',
            caption      : 'caption',
            // header class names
            header       : 'bootstrap-header', // give the header a gradient background (theme.bootstrap_2.css)
            sortNone     : '',
            sortAsc      : '',
            sortDesc     : '',
            active       : '', // applied when column is sorted
            hover        : '', // custom css required - a defined bootstrap style may not override other classes
            // icon class names
            icons        : '', // add 'icon-white' to make them white; this icon class is added to the <i> in the header
            iconSortNone : 'fa fa-sort', // class name added to icon when column is not sorted
            iconSortAsc  : 'fa fa-sort-asc', // class name added to icon when column has ascending sort
            iconSortDesc : 'fa fa-sort-desc', // class name added to icon when column has descending sort
            filterRow    : '', // filter row class
            footerRow    : '',
            footerCells  : '',
            even         : '', // even row zebra striping
            odd          : ''  // odd row zebra striping
        }
    };


})(jQuery);
