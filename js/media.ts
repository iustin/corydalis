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

// Helpers for playing media (movies) in the viewer.

/**
 * Returns the message to show the user when a movie's `play()` call fails,
 * or null if the failure needs no message.
 *
 * An `AbortError` means the play request was interrupted, either by a
 * `pause()` call or by the source changing because the user navigated away.
 * Both are user actions, so there is nothing to report.
 */
export function playFailureMessage(err: unknown): string | null {
  if (err instanceof DOMException) {
    switch (err.name) {
      case 'AbortError':
        return null;
      case 'NotAllowedError':
        return 'The browser did not allow playing the movie.';
      case 'NotSupportedError':
        return 'The browser cannot play this movie (unsupported format or failed load).';
    }
  }
  const detail = err instanceof Error ? err.message : String(err);
  return `Cannot play the movie: ${detail}`;
}

/**
 * The message to show when the movie itself can't be loaded (e.g. the
 * server answers with an error).
 *
 * The movie's URL is on a `<source>` child of the video element, and
 * when that fails, the browser only fires an `error` event on the source
 * element: `play()` neither resolves nor rejects, as the element waits
 * for another source to be added.
 */
export const LOAD_FAILURE_MESSAGE = 'The movie could not be loaded.';
