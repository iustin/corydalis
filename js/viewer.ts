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

// The image viewer's entry point: starts the viewer (js/viewer-app.ts)
// with the browser-only libraries, once the page is ready.

import screenfull from 'screenfull';
import 'bootstrap';
import { Modal } from 'bootstrap';
import { whenReady } from './ready';
import { startViewer } from './viewer-app';

whenReady(function () {
  startViewer(window, {
    helpModal: (element) => new Modal(element, { backdrop: true }),
    fullScreen: screenfull,
  });
});
