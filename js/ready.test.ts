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

import { whenReady } from './ready.ts';

describe('whenReady', () => {
  it('runs right away in a parsed document', () => {
    const doc = new JSDOM('').window.document;
    let runs = 0;
    whenReady(() => runs++, doc);
    assert.equal(runs, 1);
  });

  it('waits for a document still loading, running once', () => {
    let readyState: DocumentReadyState = 'loading';
    const target = new EventTarget();
    const doc = {
      get readyState() {
        return readyState;
      },
      addEventListener: target.addEventListener.bind(target),
    } as Pick<Document, 'readyState' | 'addEventListener'>;
    let runs = 0;
    whenReady(() => runs++, doc);
    assert.equal(runs, 0);
    readyState = 'interactive';
    target.dispatchEvent(new Event('DOMContentLoaded'));
    target.dispatchEvent(new Event('DOMContentLoaded'));
    assert.equal(runs, 1);
  });
});
