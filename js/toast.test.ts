import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import {
  TOAST_DELAY,
  TOAST_STYLES,
  type ToastKind,
  parseToastKind,
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
