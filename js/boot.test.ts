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
import { describe, it, type TestContext } from 'node:test';

import { JSDOM } from 'jsdom';

import { bootData, bootJSON, loadBootJSON } from './boot.ts';

/** A document with a boot element having the given attributes. */
function page(attrs: string): Document {
  return new JSDOM(`<div id="boot" ${attrs}></div>`).window.document;
}

/** A fake fetch, answering with the given body, recording the URLs. */
function fakeFetch(answer: () => Promise<Response>) {
  const urls: string[] = [];
  const fetch = (url: string) => {
    urls.push(url);
    return answer();
  };
  return { fetch: fetch as typeof globalThis.fetch, urls };
}

/** Silences console.error, returning its mock (to read its calls). */
function errors(t: TestContext) {
  return t.mock.method(console, 'error', () => {}).mock;
}

describe('bootData', () => {
  it('reads data attributes of the boot element', () => {
    const doc = page('data-random-url="/random?q=1&amp;r=2"');
    assert.equal(bootData('random-url', doc), '/random?q=1&r=2');
  });
  it('returns null for missing attributes', () => {
    assert.equal(bootData('other', page('data-x="1"')), null);
  });
  it('returns null without a boot element', () => {
    const doc = new JSDOM('<div data-x="1"></div>').window.document;
    assert.equal(bootData('x', doc), null);
  });
});

describe('bootJSON', () => {
  it('parses JSON values', () => {
    const doc = page(
      `data-debug="false" data-view-info='{"name":"a","n":[1]}'`,
    );
    assert.equal(bootJSON('debug', doc), false);
    assert.deepEqual(bootJSON('view-info', doc), { name: 'a', n: [1] });
  });
  it('returns null for missing attributes', () => {
    assert.equal(bootJSON('debug', page('')), null);
  });
  it('fails on invalid JSON', () => {
    assert.throws(() => bootJSON('debug', page('data-debug="True"')));
  });
});

describe('loadBootJSON', () => {
  const env = (fetch: typeof globalThis.fetch) => ({
    fetch,
    pageUrl: 'https://cory.example/',
  });

  it('loads the URL given in the boot data', async () => {
    const { fetch, urls } = fakeFetch(() =>
      Promise.resolve(new Response('{"a": 1}')),
    );
    const got: unknown[] = [];
    await loadBootJSON(
      'stats-url',
      (d) => got.push(d),
      page('data-stats-url="/stats"'),
      env(fetch),
    );
    assert.deepEqual(urls, ['/stats']);
    assert.deepEqual(got, [{ a: 1 }]);
  });

  it('logs failed requests', async (t) => {
    const mock = errors(t);
    const { fetch } = fakeFetch(() =>
      Promise.resolve(new Response('', { status: 500 })),
    );
    const got: unknown[] = [];
    await loadBootJSON(
      'stats-url',
      (d) => got.push(d),
      page('data-stats-url="/stats"'),
      env(fetch),
    );
    assert.deepEqual(got, []);
    assert.equal(mock.callCount(), 1);
    assert.match(String(mock.calls[0].arguments[0]), /\/stats/);
  });

  it('logs a missing URL, without a request', async (t) => {
    const mock = errors(t);
    const { fetch, urls } = fakeFetch(() => Promise.reject(new Error('no')));
    await loadBootJSON('stats-url', () => {}, page(''), env(fetch));
    assert.deepEqual(urls, []);
    assert.equal(mock.callCount(), 1);
  });
});
