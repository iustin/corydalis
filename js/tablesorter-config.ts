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
    widgets: ['uitheme', 'filter', 'group'],
    widgetOptions: {
      group_collapsible: false, // make the group header clickable and collapse the rows below it.
      group_collapsed: false, // start with all groups collapsed (if true)
      group_saveGroups: false, // remember collapsed groups
      group_count: ' ({num})', // if not false, the "{num}" string is replaced with the number of rows in the group
      // group header text added for invalid dates
      group_dateInvalid: 'Invalid Date',
    },
  });

  // Initialize tablesorter on DOM ready
  $(function () {
    $('.tablesorter').tablesorter();
  });
}
