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

// JSON requests to the server. All requests from the frontend go through
// here (the linter forbids using fetch directly elsewhere), so that
// state-changing ones always carry the CSRF token, see csrf.ts.

// With the extension, as the unit tests run this file directly in Node.
import { CSRF_HEADER, csrfTokenFromPage, needsCsrfToken } from './csrf.ts';

/** The HTTP methods the frontend uses. */
export type Method = 'GET' | 'PUT' | 'DELETE';

/**
 * A failed request.
 *
 * - `status` is the HTTP status, or 0 if there was no response (e.g. a
 *   network error);
 * - `kind` is `error` for a failed request (a network error, or an HTTP
 *   status other than 2xx), or `parsererror` if the reply wasn't JSON;
 * - the message holds the details: the HTTP status text, or the error.
 */
export class RequestError extends Error {
  readonly status: number;
  readonly kind: 'error' | 'parsererror';

  constructor(status: number, kind: 'error' | 'parsererror', details: string) {
    super(details);
    this.name = 'RequestError';
    this.status = status;
    this.kind = kind;
  }
}

/** Dependencies of requestJSON, replaceable in the tests. */
export type RequestEnv = {
  /** The fetch implementation; defaults to the browser's. */
  fetch?: typeof fetch;
  /** The URL of the page; defaults to the document's base URL. */
  pageUrl?: string;
  /** Returns the CSRF token; defaults to the one in the page. */
  csrfToken?: () => string | null;
};

function errorText(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}

/**
 * Sends a request expecting a JSON reply, and returns the parsed reply.
 *
 * State-changing requests to the page's origin carry the CSRF token. The
 * request asks for JSON (via the Accept header), so that the server
 * replies with JSON rather than HTML, also for errors such as a missing
 * session (401).
 *
 * @throws RequestError if the request fails, or the reply isn't JSON
 */
export async function requestJSON<T>(
  method: Method,
  url: string,
  env: RequestEnv = {},
): Promise<T> {
  const headers: Record<string, string> = { Accept: 'application/json' };
  if (needsCsrfToken(method, url, env.pageUrl ?? document.baseURI)) {
    const token = (env.csrfToken ?? csrfTokenFromPage)();
    if (token !== null) {
      headers[CSRF_HEADER] = token;
    }
  }
  const doFetch = env.fetch ?? fetch;
  let response: Response;
  try {
    response = await doFetch(url, {
      method,
      headers,
      credentials: 'same-origin',
    });
  } catch (err) {
    throw new RequestError(0, 'error', errorText(err));
  }
  if (!response.ok) {
    throw new RequestError(response.status, 'error', response.statusText);
  }
  try {
    return (await response.json()) as T;
  } catch (err) {
    throw new RequestError(response.status, 'parsererror', errorText(err));
  }
}

/** Gets a JSON document, see requestJSON. */
export function getJSON<T>(url: string, env: RequestEnv = {}): Promise<T> {
  return requestJSON<T>('GET', url, env);
}
