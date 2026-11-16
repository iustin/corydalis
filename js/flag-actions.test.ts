import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import {
  type ElementLike,
  flagActionFor,
  parseFlagValue,
} from './flag-actions.ts';

/** A minimal fake element, with attributes, a parent and children. */
class FakeElement implements ElementLike {
  parent: FakeElement | null = null;
  children: FakeElement[] = [];
  attrs: Record<string, string>;
  className: string;
  // No parameter properties: Node's type stripping doesn't support them.
  constructor(attrs: Record<string, string> = {}, className = '') {
    this.attrs = attrs;
    this.className = className;
  }
  add(child: FakeElement): FakeElement {
    child.parent = this;
    this.children.push(child);
    return child;
  }
  /** Supports only `[attr]` and `.class` selectors. */
  matches(selector: string): boolean {
    const attr = /^\[([\w-]+)\]$/.exec(selector);
    if (attr) {
      return attr[1] in this.attrs;
    }
    return selector.startsWith('.') && this.className === selector.slice(1);
  }
  closest(selector: string): FakeElement | null {
    if (this.matches(selector)) {
      return this;
    }
    return this.parent?.closest(selector) ?? null;
  }
  getAttribute(name: string): string | null {
    return this.attrs[name] ?? null;
  }
  querySelector(selector: string): FakeElement | null {
    for (const child of this.children) {
      if (child.matches(selector)) {
        return child;
      }
      const found = child.querySelector(selector);
      if (found) {
        return found;
      }
    }
    return null;
  }
}

/** Builds the widget rendered by the server, returning its parts. */
function widget(url = '/image/flag/a/b') {
  const container = new FakeElement({ 'data-flag-url': url });
  const flagButton = container.add(new FakeElement({ 'data-flag': 'true' }));
  const unflagButton = container.add(new FakeElement({ 'data-flag': 'false' }));
  const status = container.add(new FakeElement({}, 'flag-status'));
  return { container, flagButton, unflagButton, status };
}

describe('parseFlagValue', () => {
  it('parses the boolean values', () => {
    assert.equal(parseFlagValue('true'), true);
    assert.equal(parseFlagValue('false'), false);
  });
  for (const value of [null, '', 'yes', 'TRUE', '1']) {
    it(`rejects ${JSON.stringify(value)}`, () => {
      assert.equal(parseFlagValue(value), null);
    });
  }
});

describe('flagActionFor', () => {
  it('flags via the flag button', () => {
    const { flagButton, status } = widget();
    assert.deepEqual(flagActionFor(flagButton), {
      url: '/image/flag/a/b',
      flag: true,
      status,
    });
  });
  it('un-flags via the un-flag button', () => {
    const { unflagButton, status } = widget();
    assert.deepEqual(flagActionFor(unflagButton), {
      url: '/image/flag/a/b',
      flag: false,
      status,
    });
  });
  it('ignores buttons outside a widget', () => {
    assert.equal(flagActionFor(new FakeElement({ 'data-flag': 'true' })), null);
  });
  it('ignores buttons with an invalid flag value', () => {
    const container = new FakeElement({ 'data-flag-url': '/x' });
    const button = container.add(new FakeElement({ 'data-flag': 'yes' }));
    assert.equal(flagActionFor(button), null);
  });
  it('ignores widgets without a URL', () => {
    assert.equal(flagActionFor(widget('').flagButton), null);
  });
  it('works without a status element', () => {
    const container = new FakeElement({ 'data-flag-url': '/x' });
    const button = container.add(new FakeElement({ 'data-flag': 'false' }));
    assert.deepEqual(flagActionFor(button), {
      url: '/x',
      flag: false,
      status: null,
    });
  });
});
