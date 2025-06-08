// PageTable bundle
import $ from 'jquery';
import 'bootstrap';
import 'tablesorter';
import { setupTablesorter } from 'tablesorter-config';

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
