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

import { csrfTokenFromPage, isCsrfRejection, needsCsrfToken } from './csrf.ts';

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

describe('needsCsrfToken', () => {
  const PAGE = 'https://cory.example/folder/a';

  for (const method of ['PUT', 'DELETE', 'POST', 'put']) {
    it(`requires the token on ${method}`, () => {
      assert.equal(needsCsrfToken(method, '/image/flag/a/b', PAGE), true);
    });
  }
  for (const method of ['GET', 'HEAD', 'OPTIONS', 'TRACE', 'get']) {
    it(`does not require the token on ${method}`, () => {
      assert.equal(needsCsrfToken(method, '/x', PAGE), false);
    });
  }
  it('accepts absolute and relative URLs on the same origin', () => {
    assert.equal(needsCsrfToken('PUT', 'https://cory.example/x', PAGE), true);
    assert.equal(needsCsrfToken('PUT', 'x', PAGE), true);
  });
  it('never sends the token to other origins', () => {
    for (const url of [
      'https://other.example/x',
      'http://cory.example/x',
      'https://cory.example:8443/x',
      '//other.example/x',
    ]) {
      assert.equal(needsCsrfToken('PUT', url, PAGE), false, url);
    }
  });
});
