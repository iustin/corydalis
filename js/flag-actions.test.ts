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
  type ElementLike,
  flagActionFor,
  notifyReply,
  parseFlagValue,
} from './flag-actions.ts';

/** A minimal fake element, with attributes and a parent. */
class FakeElement implements ElementLike {
  parent: FakeElement | null = null;
  attrs: Record<string, string>;
  // No parameter properties: Node's type stripping doesn't support them.
  constructor(attrs: Record<string, string> = {}) {
    this.attrs = attrs;
  }
  add(child: FakeElement): FakeElement {
    child.parent = this;
    return child;
  }
  /** Supports only `[attr]` selectors. */
  matches(selector: string): boolean {
    const attr = /^\[([\w-]+)\]$/.exec(selector);
    return attr !== null && attr[1] in this.attrs;
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
}

/** Builds the widget rendered by the server, returning its parts. */
function widget(url = '/image/flag/a/b') {
  const container = new FakeElement({ 'data-flag-url': url });
  const flagButton = container.add(new FakeElement({ 'data-flag': 'true' }));
  const unflagButton = container.add(new FakeElement({ 'data-flag': 'false' }));
  return { container, flagButton, unflagButton };
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
    assert.deepEqual(flagActionFor(widget().flagButton), {
      url: '/image/flag/a/b',
      flag: true,
    });
  });
  it('un-flags via the un-flag button', () => {
    assert.deepEqual(flagActionFor(widget().unflagButton), {
      url: '/image/flag/a/b',
      flag: false,
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
});

describe('notifyReply', () => {
  /** Returns the notifications shown for a reply. */
  function notificationsFor(reply: { text: string; kind?: string }) {
    const shown: [string, string][] = [];
    notifyReply((text, kind) => shown.push([text, kind]), reply);
    return shown;
  }

  it('shows the message with its kind', () => {
    assert.deepEqual(
      notificationsFor({ text: 'Image flagged', kind: 'success' }),
      [['Image flagged', 'success']],
    );
    assert.deepEqual(
      notificationsFor({ text: 'Image already flagged!', kind: 'warning' }),
      [['Image already flagged!', 'warning']],
    );
  });
  it('shows messages of unknown or missing kinds as info', () => {
    assert.deepEqual(notificationsFor({ text: 'x', kind: 'bogus' }), [
      ['x', 'info'],
    ]);
    assert.deepEqual(notificationsFor({ text: 'y' }), [['y', 'info']]);
  });
});
