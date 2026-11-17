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

import { JSDOM } from 'jsdom';

import {
  CSRF_HEADER,
  csrfTokenFromPage,
  isCsrfRejection,
  setupCsrf,
} from './csrf.ts';

/** A document whose head holds the given meta tags (name to content). */
function page(metas: Record<string, string>): Document {
  const doc = new JSDOM().window.document;
  for (const [name, content] of Object.entries(metas)) {
    const meta = doc.createElement('meta');
    meta.name = name;
    meta.content = content;
    doc.head.append(meta);
  }
  return doc;
}

describe('csrfTokenFromPage', () => {
  it('reads the token from the meta tag', () => {
    assert.equal(csrfTokenFromPage(page({ 'csrf-token': 'abc123' })), 'abc123');
  });
  it('returns null if there is no meta tag', () => {
    assert.equal(csrfTokenFromPage(page({})), null);
    assert.equal(csrfTokenFromPage(page({ other: 'abc123' })), null);
  });
});

describe('isCsrfRejection', () => {
  it('recognises the CSRF check rejection', () => {
    assert.equal(isCsrfRejection(403), true);
  });
  for (const status of [0, 200, 401, 404, 500]) {
    it(`does not treat ${status} as a CSRF rejection`, () => {
      assert.equal(isCsrfRejection(status), false);
    });
  }
});

describe('setupCsrf', () => {
  /** Installs the prefilter with the given token source, and returns it. */
  function installPrefilter(getToken) {
    let prefilter;
    setupCsrf(
      {
        ajaxPrefilter: (fn) => {
          prefilter = fn;
        },
      },
      getToken,
    );
    return prefilter;
  }

  /** Runs a request through the prefilter, returning its headers. */
  function headersFor(options, token: string | null = 'abc123') {
    const prefilter = installPrefilter(() => token);
    const headers = {};
    prefilter(options, options, {
      setRequestHeader: (name, value) => {
        headers[name] = value;
      },
    });
    return headers;
  }

  for (const type of ['PUT', 'DELETE', 'POST', 'put']) {
    it(`sends the token on ${type}`, () => {
      assert.deepEqual(headersFor({ type }), { [CSRF_HEADER]: 'abc123' });
    });
  }
  for (const type of ['GET', 'HEAD', 'OPTIONS', 'TRACE', 'get', undefined]) {
    it(`does not send the token on ${type}`, () => {
      assert.deepEqual(headersFor({ type }), {});
    });
  }
  it('does not send the token to other origins', () => {
    assert.deepEqual(headersFor({ type: 'PUT', crossDomain: true }), {});
  });
  it('sends no header if there is no token', () => {
    assert.deepEqual(headersFor({ type: 'PUT' }, null), {});
  });
});
