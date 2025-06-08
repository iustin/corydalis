import $ from 'jquery';
import 'bootstrap';
import 'tablesorter';
import { setupTablesorter } from 'tablesorter-config';

// Import page initializers
import { initCameraStats } from 'camerastats';
import { initLensStats } from 'lensstats';

// Create a mapping of initializer functions
const initializers = {
  'camera-stats': initCameraStats,
  //  'camera-info': initCameraInfo,
  'lens-stats': initLensStats,
  //  'lens-info': initLensInfo,
  //  'curate': initCurate
};

function runInitializers() {
  // Look for boot element with data-init attribute
  const bootElement = document.getElementById('boot');
  if (bootElement && bootElement.hasAttribute('data-init')) {
    const initType = bootElement.getAttribute('data-init');
    if (initType && initializers[initType]) {
      initializers[initType]();
    }
  }
}

// Initialize on DOM ready.
if (document.readyState === 'loading') {
  // Document still loading, add event listener
  document.addEventListener('DOMContentLoaded', runInitializers);
} else {
  // Document already loaded, run the function
  runInitializers();
}
