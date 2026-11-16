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
