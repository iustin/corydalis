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

// The page's boot data: the server passes page-specific data (URLs,
// settings) to the scripts via `data-*` attributes of the `#boot` element.

// With the extension, as the unit tests run this file directly in Node.
import { type RequestEnv, getJSON } from './http.ts';

/**
 * Returns the value of the boot element's `data-<name>` attribute, or
 * null if there is no boot element or no such attribute.
 */
export function bootData(
  name: string,
  doc: Pick<Document, 'getElementById'> = document,
): string | null {
  return doc.getElementById('boot')?.getAttribute(`data-${name}`) ?? null;
}

/**
 * Returns the JSON value of the boot element's `data-<name>` attribute
 * (which the server encodes as JSON), or null if it is missing.
 *
 * @throws SyntaxError if the value is not valid JSON
 */
export function bootJSON<T>(
  name: string,
  doc: Pick<Document, 'getElementById'> = document,
): T | null {
  const value = bootData(name, doc);
  return value === null ? null : (JSON.parse(value) as T);
}

/**
 * Gets the JSON document at the URL in the boot element's `data-<name>`
 * attribute, and passes it to the callback. Failures (including a
 * missing attribute) are logged to the console. Never fails.
 */
export function loadBootJSON<T>(
  name: string,
  onData: (data: T) => void,
  doc: Pick<Document, 'getElementById'> = document,
  env: RequestEnv = {},
): Promise<void> {
  const url = bootData(name, doc);
  if (url === null) {
    console.error(`Missing boot data: ${name}`);
    return Promise.resolve();
  }
  return getJSON<T>(url, env).then(onData, (err: unknown) => {
    console.error(`Failed to load ${url}:`, err);
  });
}
