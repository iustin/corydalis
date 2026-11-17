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

import { CSRF_HEADER } from './csrf.ts';
import { RequestError, getJSON, requestJSON } from './http.ts';

const PAGE = 'https://cory.example/folder/a';

type Sent = { url: string; init: RequestInit };

/**
 * A fake fetch, answering every request with the given response (or
 * failing with the given error), and recording the requests in `sent`.
 */
function fakeFetch(answer: () => Response | Error) {
  const sent: Sent[] = [];
  const fetch = (url: string, init?: RequestInit) => {
    sent.push({ url: url, init: init ?? {} });
    const result = answer();
    return result instanceof Error
      ? Promise.reject(result)
      : Promise.resolve(result);
  };
  return { fetch: fetch as unknown as typeof globalThis.fetch, sent };
}

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { 'Content-Type': 'application/json' },
  });

/** Sends a request with a fake fetch, returning the request sent. */
async function sentFor(
  method: 'GET' | 'PUT' | 'DELETE',
  url: string,
  token: string | null = 'abc123',
): Promise<Sent> {
  const { fetch, sent } = fakeFetch(() => json({}));
  await requestJSON(method, url, {
    fetch,
    pageUrl: PAGE,
    csrfToken: () => token,
  });
  assert.equal(sent.length, 1);
  return sent[0];
}

const headers = (s: Sent) => s.init.headers as Record<string, string>;

/** Returns the error a request fails with, given the server's answer. */
async function failureFor(answer: () => Response | Error) {
  const { fetch } = fakeFetch(answer);
  try {
    await getJSON('/x', { fetch, pageUrl: PAGE });
  } catch (err) {
    assert.ok(err instanceof RequestError);
    return { status: err.status, kind: err.kind, details: err.message };
  }
  assert.fail('the request did not fail');
}

describe('requestJSON', () => {
  it('returns the parsed reply', async () => {
    const { fetch } = fakeFetch(() => json({ text: 'hi', n: [1, 2] }));
    const reply = await getJSON('/x', { fetch, pageUrl: PAGE });
    assert.deepEqual(reply, { text: 'hi', n: [1, 2] });
  });

  it('sends the method, asks for JSON, and keeps the session', async () => {
    const sent = await sentFor('PUT', '/image/flag/a/b');
    assert.equal(sent.url, '/image/flag/a/b');
    assert.equal(sent.init.method, 'PUT');
    assert.equal(headers(sent).Accept, 'application/json');
    assert.equal(sent.init.credentials, 'same-origin');
  });

  for (const method of ['PUT', 'DELETE'] as const) {
    it(`sends the CSRF token on ${method}`, async () => {
      const sent = await sentFor(method, '/x');
      assert.equal(headers(sent)[CSRF_HEADER], 'abc123');
    });
  }

  it('does not send the CSRF token on GET', async () => {
    const sent = await sentFor('GET', '/x');
    assert.equal(headers(sent)[CSRF_HEADER], undefined);
  });

  it('does not send the CSRF token to other origins', async () => {
    const sent = await sentFor('PUT', 'https://other.example/x');
    assert.equal(headers(sent)[CSRF_HEADER], undefined);
  });

  it('sends no CSRF header if the page has no token', async () => {
    const sent = await sentFor('PUT', '/x', null);
    assert.equal(CSRF_HEADER in headers(sent), false);
  });

  it('fails with the HTTP status on an error reply', async () => {
    assert.deepEqual(
      await failureFor(
        () => new Response('no', { status: 403, statusText: 'Forbidden' }),
      ),
      { status: 403, kind: 'error', details: 'Forbidden' },
    );
  });

  it('fails with status 0 on a network error', async () => {
    assert.deepEqual(await failureFor(() => new TypeError('Failed to fetch')), {
      status: 0,
      kind: 'error',
      details: 'Failed to fetch',
    });
  });

  it('fails with a parser error if the reply is not JSON', async () => {
    const failure = await failureFor(
      () => new Response('<html>', { status: 200 }),
    );
    assert.equal(failure.status, 200);
    assert.equal(failure.kind, 'parsererror');
  });
});
