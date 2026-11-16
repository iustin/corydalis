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

// PageTable bundle
import $ from 'jquery';
import 'bootstrap';
import 'tablesorter';
// Import the widgets file that contains the grouping widget.
import 'tablesorter/dist/js/widgets/widget-grouping.min.js';
import { setupCsrf } from './csrf';
import { setupFlagActions } from './flag-actions';
import { notify } from './notify';
import { setupTablesorter } from './tablesorter-config';

setupCsrf($);
setupFlagActions($, notify);

function runInitializer(): void {
  setupTablesorter();
}

// Initialize on DOM ready.
if (document.readyState === 'loading') {
  // Document still loading, add event listener
  document.addEventListener('DOMContentLoaded', runInitializer);
} else {
  // Document already loaded, run the function
  runInitializer();
}
