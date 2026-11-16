/* Copyright (C) 2013 Iustin Pop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

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
    // Note: the filter widget builds the options of `.filter-select`
    // columns as HTML from the cell texts (or `data-text`), without
    // escaping them. Only use `.filter-select` on columns whose values are
    // fixed by the server code, never on file names or EXIF data.
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
