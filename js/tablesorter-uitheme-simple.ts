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

/**
 * Configure tablesorter bootstrap theme with FontAwesome icons
 * Simplified version of bootstrap-theme in widget-uitheme.js
 */
export function configureTablesorterTheme(): void {
  const ts = $.tablesorter || {};

  $.extend(ts.themes.bootstrap, {
    table: 'table',
    // add 'icon-white' to make them white; this icon class is added
    // to the <i> in the header
    icons: '',
    // class name added to icon when column is not sorted
    iconSortNone: 'fa-solid fa-sort',
    // class name added to icon when column has ascending sort
    iconSortAsc: 'fa-solid fa-sort-up',
    // class name added to icon when column has descending sort
    iconSortDesc: 'fa-solid fa-sort-down',
  });
}
