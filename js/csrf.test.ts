import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import {
  CSRF_HEADER,
  csrfTokenFromPage,
  isCsrfRejection,
  setupCsrf,
} from './csrf.ts';

/** A minimal stand-in for a document, holding the given meta tags. */
function fakePage(metas) {
  return {
    querySelector: (selector) => {
      const match = /^meta\[name="([^"]+)"\]$/.exec(selector);
      return match && match[1] in metas ? { content: metas[match[1]] } : null;
    },
  };
}

describe('csrfTokenFromPage', () => {
  it('reads the token from the meta tag', () => {
    assert.equal(
      csrfTokenFromPage(fakePage({ 'csrf-token': 'abc123' })),
      'abc123',
    );
  });
  it('returns null if there is no meta tag', () => {
    assert.equal(csrfTokenFromPage(fakePage({})), null);
    assert.equal(csrfTokenFromPage(fakePage({ other: 'abc123' })), null);
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
