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
