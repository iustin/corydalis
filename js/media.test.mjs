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
      playFailureMessage(new DOMException('denied', 'NotAllowedError')),
      /did not allow/,
    );
  });
  it('explains unsupported or failed sources', () => {
    assert.match(
      playFailureMessage(new DOMException('bad source', 'NotSupportedError')),
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
