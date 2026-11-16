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

import { playFailureMessage } from './media.ts';

describe('playFailureMessage', () => {
  it('ignores interrupted play requests', () => {
    assert.equal(
      playFailureMessage(new DOMException('interrupted', 'AbortError')),
      null,
    );
  });
  it('explains blocked playback', () => {
    assert.match(
      // A null result fails the match, as intended.
      playFailureMessage(new DOMException('denied', 'NotAllowedError')) ?? '',
      /did not allow/,
    );
  });
  it('explains unsupported or failed sources', () => {
    assert.match(
      playFailureMessage(new DOMException('bad source', 'NotSupportedError')) ??
        '',
      /cannot play this movie/,
    );
  });
  it('includes the details of other DOM exceptions', () => {
    assert.equal(
      playFailureMessage(new DOMException('weird', 'SomethingElseError')),
      'Cannot play the movie: weird',
    );
  });
  it('includes the details of plain errors', () => {
    assert.equal(
      playFailureMessage(new Error('boom')),
      'Cannot play the movie: boom',
    );
  });
  it('handles non-error rejection values', () => {
    assert.equal(
      playFailureMessage('just a string'),
      'Cannot play the movie: just a string',
    );
  });
});
