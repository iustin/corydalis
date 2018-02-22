;(function ($) {
    'use strict';
    var ts = $.tablesorter || {};

    $.extend(true, ts.defaults, {
        theme: "bootstrap",
        headerTemplate: "{content} {icon}",
        widgets : [ "uitheme", "filter" ],
        textExtraction: {
            '.data-sort-value': function(node, table, cellIndex) {
                return $(node).data("sortValue");
            },
        }
    });
})(jQuery);
