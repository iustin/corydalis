;(function ($) {
    'use strict';
    var ts = $.tablesorter || {};

    $.extend(true, ts.defaults, {
        theme: "bootstrap",
        headerTemplate: "{content} {icon}",
        widgets : [ "uitheme", "filter" ],
    });
})(jQuery);
