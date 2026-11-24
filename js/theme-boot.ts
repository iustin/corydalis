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

// Theme entry point (see js/theme.ts): loaded as a classic, blocking
// script in every page's head, so that the theme is set before the page
// is shown.
import { DARK_QUERY, setupTheme } from './theme';

/** The local storage, unless the browser refuses access to it. */
function localStorageOrNull(): Storage | null {
  try {
    return window.localStorage;
  } catch {
    return null;
  }
}

setupTheme({
  doc: document,
  storage: localStorageOrNull(),
  media: window.matchMedia?.(DARK_QUERY) ?? null,
});
