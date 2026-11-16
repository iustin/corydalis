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

// Flagging and un-flagging images for review.

// With the extension, as the unit tests run this file directly in Node.
import { isCsrfRejection } from './csrf.ts';

/**
 * The reply of the server to a flag request: a message, and its kind (e.g.
 * success, or warning if the image was already flagged).
 */
export type FlagReply = { text: string; kind?: string };

/**
 * Sends a request to flag (or un-flag) an image, using the image's flag URL.
 *
 * The request asks for JSON, so that the server replies with a message
 * instead of redirecting to the image page.
 */
export function sendFlag(
  jquery: Pick<JQueryStatic, 'ajax'>,
  url: string,
  flag: boolean,
): JQuery.jqXHR<FlagReply> {
  return jquery.ajax({
    url,
    type: flag ? 'PUT' : 'DELETE',
    dataType: 'json',
  });
}

/** Returns the message to show the user when a flag request failed. */
export function flagFailureMessage(
  httpStatus: number,
  status: string,
  details: string,
): string {
  return isCsrfRejection(httpStatus)
    ? 'Could not flag the image: this page is out of date, ' +
        'please reload it and try again.'
    : 'Error flagging image: ' + status + ', details: ' + details;
}
