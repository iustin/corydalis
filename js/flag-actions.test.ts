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
  runFlagAction,
} from './flag-actions.ts';
import { type FlagReply, flagFailureMessage } from './flag.ts';

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

describe('runFlagAction', () => {
  type Outcome =
    | { reply: FlagReply }
    | { httpStatus: number; status: string; details: string };

  /**
   * Runs a flag action against a fake jQuery whose request has the given
   * outcome, returning the requests sent and the notifications shown.
   */
  function run(outcome: Outcome, flag = true) {
    const requests: JQuery.AjaxSettings[] = [];
    const shown: [string, string][] = [];
    // The input: a fake of the request object (jqXHR) that jQuery returns,
    // standing in for the server's answer. It mirrors almost 1:1 what
    // jQuery does in a real run, where done() and fail() register
    // callbacks, and jQuery later calls the matching one when the answer
    // arrives. The fake knows the outcome up front, so it calls the
    // matching callback immediately (synchronously), and decides which
    // path runFlagAction takes.
    const xhr = {
      done(callback: (reply: FlagReply) => void) {
        if ('reply' in outcome) {
          callback(outcome.reply);
        }
        return xhr;
      },
      fail(
        callback: (
          xhr: { status: number },
          status: string,
          details: string,
        ) => void,
      ) {
        if (!('reply' in outcome)) {
          callback(
            { status: outcome.httpStatus },
            outcome.status,
            outcome.details,
          );
        }
        return xhr;
      },
    };
    // The outputs: the fake jQuery and the notifier below record what
    // runFlagAction calls, i.e. the request sent (in `requests`) and the
    // notifications shown (in `shown`), for the tests to check.
    const jquery = {
      ajax: (settings: JQuery.AjaxSettings) => {
        requests.push(settings);
        return xhr;
      },
    } as unknown as Pick<JQueryStatic, 'ajax'>;
    runFlagAction(jquery, (text, kind) => shown.push([text, kind]), {
      url: '/image/flag/a/b',
      flag,
    });
    return { requests, shown };
  }

  it('sends the request of the action', () => {
    const { requests } = run({ reply: { text: 'x' } }, false);
    assert.equal(requests.length, 1);
    assert.equal(requests[0].url, '/image/flag/a/b');
    assert.equal(requests[0].type, 'DELETE');
  });
  it("shows the server's reply with its kind", () => {
    const { shown } = run({ reply: { text: 'Image flagged', kind: 'success' } });
    assert.deepEqual(shown, [['Image flagged', 'success']]);
  });
  for (const [httpStatus, status, details] of [
    [403, 'error', 'Forbidden'],
    [404, 'error', 'Not Found'],
    [0, 'timeout', ''],
  ] as const) {
    it(`shows a failure (${httpStatus}, ${status}) as an error`, () => {
      const { shown } = run({ httpStatus, status, details });
      assert.deepEqual(shown, [
        [flagFailureMessage(httpStatus, status, details), 'danger'],
      ]);
    });
  }
});
