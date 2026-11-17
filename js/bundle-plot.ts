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

import 'bootstrap';
import { setupSortableTables } from './tablesort';

// Import page initializers
import { initCameraStats } from './camerastats';
import { initLensStats } from './lensstats';
import { initCameraInfo } from './camerainfo';
import { initLensInfo } from './lensinfo';
import { initCurate } from './curate';

// Create a mapping of initializer functions
const initializers = {
  'camera-stats': initCameraStats,
  'camera-info': initCameraInfo,
  'lens-stats': initLensStats,
  'lens-info': initLensInfo,
  curate: initCurate,
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

setupSortableTables();
