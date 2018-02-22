;(function ($) {
    'use strict';
    var ts = $.tablesorter || {};

    $.extend(true, ts.defaults, {
      textExtraction: {
          '.data-sort-value': function(node, table, cellIndex) {
              return $(node).data("sortValue");
          },
      }
    });
})(jQuery);
