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

// Running code once the page is ready.

/** Runs the function once the document is parsed. */
export function whenReady(
  fn: () => void,
  doc: Pick<Document, 'readyState' | 'addEventListener'> = document,
): void {
  if (doc.readyState === 'loading') {
    doc.addEventListener('DOMContentLoaded', fn, { once: true });
  } else {
    fn();
  }
}
