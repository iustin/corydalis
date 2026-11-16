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
