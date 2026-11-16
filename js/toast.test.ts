import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import {
  TOAST_DELAY,
  parseToastKind,
  toastClasses,
  toastOptions,
} from './toast.ts';

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

describe('toastClasses', () => {
  it('uses the background matching the kind', () => {
    assert.ok(toastClasses('warning').includes('text-bg-warning'));
    assert.ok(toastClasses('danger').includes('text-bg-danger'));
  });
  it('is a Bootstrap toast', () => {
    assert.ok(toastClasses('info').includes('toast'));
  });
});
