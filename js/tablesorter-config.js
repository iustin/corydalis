(function ($) {
  'use strict';
  const ts = $.tablesorter || {};

  $.extend(true, ts.defaults, {
    theme: 'bootstrap',
    headerTemplate: '{content} {icon}',
    widgets: ['uitheme', 'filter'],
  });
})(jQuery);
$(function () {
  $('.tablesorter').tablesorter();
});
