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

import {
  TOAST_DELAY,
  TOAST_STYLES,
  type ToastKind,
  parseToastKind,
  removeWhenHidden,
  toastClasses,
  toastOptions,
} from './toast.ts';

const KINDS: ToastKind[] = ['success', 'info', 'warning', 'danger'];

describe('parseToastKind', () => {
  for (const kind of ['success', 'info', 'warning', 'danger']) {
    it(`accepts ${kind}`, () => {
      assert.equal(parseToastKind(kind), kind);
    });
  }
  for (const value of [undefined, null, '', 'error', 'SUCCESS', 1, {}]) {
    it(`defaults ${JSON.stringify(value)} to info`, () => {
      assert.equal(parseToastKind(value), 'info');
    });
  }
});

describe('toastOptions', () => {
  for (const kind of ['success', 'info', 'warning'] as const) {
    it(`hides ${kind} notifications after a delay`, () => {
      assert.deepEqual(toastOptions(kind), {
        autohide: true,
        delay: TOAST_DELAY,
      });
    });
  }
  it('keeps errors until closed', () => {
    assert.deepEqual(toastOptions('danger'), { autohide: false });
  });
});

// These check how the styles are applied, not what they are, so that the
// styles can be changed freely.
describe('toastClasses', () => {
  for (const kind of KINDS) {
    it(`is a Bootstrap toast, ${kind}`, () => {
      assert.ok(toastClasses(kind).includes('toast'));
    });
    it(`uses the style of the kind, ${kind}`, () => {
      for (const cls of TOAST_STYLES[kind].classes) {
        assert.ok(toastClasses(kind).includes(cls), cls);
      }
    });
  }
});

describe('TOAST_STYLES', () => {
  it('gives each kind a different look', () => {
    const looks = KINDS.map((kind) =>
      [...TOAST_STYLES[kind].classes].sort().join(' '),
    );
    assert.equal(new Set(looks).size, KINDS.length);
  });
});

/** A toast element and its Bootstrap instance, recording the calls. */
class FakeToast extends EventTarget {
  calls: string[] = [];
  remove(): void {
    this.calls.push('remove');
  }
  dispose(): void {
    this.calls.push('dispose');
  }
}

describe('removeWhenHidden', () => {
  it('keeps the notification until hidden', () => {
    const toast = new FakeToast();
    removeWhenHidden(toast, toast);
    toast.dispatchEvent(new Event('shown.bs.toast'));
    assert.deepEqual(toast.calls, []);
  });

  it('disposes of the instance, then removes the element, once', () => {
    const toast = new FakeToast();
    removeWhenHidden(toast, toast);
    toast.dispatchEvent(new Event('hidden.bs.toast'));
    toast.dispatchEvent(new Event('hidden.bs.toast'));
    assert.deepEqual(toast.calls, ['dispose', 'remove']);
  });
});
