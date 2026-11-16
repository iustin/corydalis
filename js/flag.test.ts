import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import { flagFailureMessage, sendFlag } from './flag.ts';

describe('sendFlag', () => {
  /** Runs sendFlag with a fake jQuery, returning the request options. */
  function requestFor(flag: boolean) {
    let options;
    sendFlag(
      {
        ajax: (opts) => {
          options = opts;
          return null as unknown as JQuery.jqXHR;
        },
      } as Pick<JQueryStatic, 'ajax'>,
      '/image/flag/a/b',
      flag,
    );
    return options;
  }

  it('flags with a PUT request', () => {
    assert.deepEqual(requestFor(true), {
      url: '/image/flag/a/b',
      type: 'PUT',
      dataType: 'json',
    });
  });
  it('un-flags with a DELETE request', () => {
    assert.deepEqual(requestFor(false), {
      url: '/image/flag/a/b',
      type: 'DELETE',
      dataType: 'json',
    });
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
