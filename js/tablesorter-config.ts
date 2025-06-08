import $ from 'jquery';
import { configureTablesorterTheme } from './tablesorter-uitheme-simple';

/**
 * Configure and initialize tablesorter
 */
export function setupTablesorter(): void {
  // Configure theme first
  configureTablesorterTheme();

  // Configure tablesorter defaults
  const ts = $.tablesorter;
  $.extend(true, ts.defaults, {
    theme: 'bootstrap',
    headerTemplate: '{content} {icon}',
    widgets: ['uitheme', 'filter'],
  });

  // Initialize tablesorter on DOM ready
  $(function () {
    $('.tablesorter').tablesorter();
  });
}
