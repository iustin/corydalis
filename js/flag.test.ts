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

import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import { flagErrorMessage, flagFailureMessage, sendFlag } from './flag.ts';
import { RequestError } from './http.ts';

describe('sendFlag', () => {
  /** Runs sendFlag with a fake fetch, returning the method and URL sent. */
  async function requestFor(flag: boolean) {
    const sent: [string | undefined, string][] = [];
    const fetch = (url: string, init?: RequestInit) => {
      sent.push([init?.method, url]);
      return Promise.resolve(new Response('{"text": "done"}'));
    };
    const reply = await sendFlag('/image/flag/a/b', flag, {
      fetch: fetch as typeof globalThis.fetch,
      pageUrl: 'https://cory.example/',
      csrfToken: () => 't',
    });
    assert.deepEqual(reply, { text: 'done' });
    return sent;
  }

  it('flags with a PUT request', async () => {
    assert.deepEqual(await requestFor(true), [['PUT', '/image/flag/a/b']]);
  });
  it('un-flags with a DELETE request', async () => {
    assert.deepEqual(await requestFor(false), [['DELETE', '/image/flag/a/b']]);
  });
});

describe('flagFailureMessage', () => {
  it('asks to reload the page on a CSRF rejection', () => {
    assert.match(flagFailureMessage(403, 'error', 'Forbidden'), /reload/);
  });
  it('includes the details of other errors', () => {
    assert.equal(
      flagFailureMessage(404, 'error', 'Not Found'),
      'Error flagging image: error, details: Not Found',
    );
  });
});

describe('flagErrorMessage', () => {
  it('uses the details of failed requests', () => {
    assert.equal(
      flagErrorMessage(new RequestError(404, 'error', 'Not Found')),
      flagFailureMessage(404, 'error', 'Not Found'),
    );
    assert.match(
      flagErrorMessage(new RequestError(403, 'error', 'Forbidden')),
      /reload/,
    );
    assert.equal(
      flagErrorMessage(new RequestError(200, 'parsererror', 'bad JSON')),
      'Error flagging image: parsererror, details: bad JSON',
    );
  });
  it('reports other errors too', () => {
    assert.equal(
      flagErrorMessage(new Error('oops')),
      'Error flagging image: error, details: Error: oops',
    );
  });
});
