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

import {
  flagActionFor,
  notifyReply,
  parseFlagValue,
  runFlagAction,
  type SendFlag,
  setupFlagActions,
} from './flag-actions.ts';
import { type FlagReply, flagFailureMessage } from './flag.ts';
import { RequestError } from './http.ts';

/** Returns a document holding the given HTML (in its body). */
function page(html: string): Document {
  return new JSDOM(`<body>${html}</body>`).window.document;
}

/** The flag widget as rendered by the server (see Handler/Widgets.hs). */
const WIDGET = (url: string) => `
  <span class="image-flag-actions" data-flag-url="${url}">
    <button type="button" data-flag="true">Flag</button>
    <button type="button" data-flag="false">Un-flag</button>
  </span>`;

/** Builds the widget, returning its parts. */
function widget(url = '/image/flag/a/b') {
  const doc = page(WIDGET(url));
  const [flagButton, unflagButton] = doc.querySelectorAll('button');
  return { doc, flagButton, unflagButton };
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
    const doc = page('<button data-flag="true">Flag</button>');
    assert.equal(flagActionFor(doc.querySelector('button')!), null);
  });
  it('ignores buttons with an invalid flag value', () => {
    const doc = page(
      '<span data-flag-url="/x"><b data-flag="yes">x</b></span>',
    );
    assert.equal(flagActionFor(doc.querySelector('b')!), null);
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
    | { httpStatus: number; status: 'error'; details: string };

  /**
   * Runs a flag action, with a request that has the given outcome,
   * returning the requests sent and the notifications shown.
   */
  async function run(outcome: Outcome, flag = true) {
    const requests: [string, boolean][] = [];
    const shown: [string, string][] = [];
    const send: SendFlag = (url, flag) => {
      requests.push([url, flag]);
      return 'reply' in outcome
        ? Promise.resolve(outcome.reply)
        : Promise.reject(
            new RequestError(
              outcome.httpStatus,
              outcome.status,
              outcome.details,
            ),
          );
    };
    await runFlagAction(
      (text, kind) => shown.push([text, kind]),
      { url: '/image/flag/a/b', flag },
      send,
    );
    return { requests, shown };
  }

  it('sends the request of the action', async () => {
    const { requests } = await run({ reply: { text: 'x' } }, false);
    assert.deepEqual(requests, [['/image/flag/a/b', false]]);
  });
  it("shows the server's reply with its kind", async () => {
    const { shown } = await run({
      reply: { text: 'Image flagged', kind: 'success' },
    });
    assert.deepEqual(shown, [['Image flagged', 'success']]);
  });
  for (const [httpStatus, details] of [
    [403, 'Forbidden'],
    [404, 'Not Found'],
    [0, 'Failed to fetch'],
  ] as const) {
    it(`shows a failure (${httpStatus}) as an error`, async () => {
      const { shown } = await run({ httpStatus, status: 'error', details });
      assert.deepEqual(shown, [
        [flagFailureMessage(httpStatus, 'error', details), 'danger'],
      ]);
    });
  }
});

describe('setupFlagActions', () => {
  /** A flag sender whose requests are recorded, and never answered. */
  function recordingSender() {
    const requests: [string, boolean][] = [];
    const send: SendFlag = (url, flag) => {
      requests.push([url, flag]);
      return new Promise(() => {});
    };
    return { send, requests };
  }

  /** Clicks the element, returning whether the default was prevented. */
  function click(el: Element): boolean {
    const view = el.ownerDocument.defaultView!;
    const event = new view.MouseEvent('click', {
      bubbles: true,
      cancelable: true,
    });
    el.dispatchEvent(event);
    return event.defaultPrevented;
  }

  it('sends the flag request of the clicked button', () => {
    const { doc, flagButton, unflagButton } = widget();
    const { send, requests } = recordingSender();
    setupFlagActions(() => {}, doc, send);
    assert.equal(click(flagButton), true, 'prevents the default action');
    click(unflagButton);
    assert.deepEqual(requests, [
      ['/image/flag/a/b', true],
      ['/image/flag/a/b', false],
    ]);
  });

  it('leaves malformed widgets alone', () => {
    const { doc, flagButton } = widget('');
    const { send, requests } = recordingSender();
    setupFlagActions(() => {}, doc, send);
    assert.equal(click(flagButton), false);
    assert.deepEqual(requests, []);
  });

  it('only looks inside the given root', () => {
    const doc = page(`<div id="a">${WIDGET('/a')}</div>${WIDGET('/b')}`);
    const { send, requests } = recordingSender();
    setupFlagActions(() => {}, doc.getElementById('a')!, send);
    for (const button of doc.querySelectorAll('[data-flag="true"]')) {
      click(button);
    }
    assert.deepEqual(requests, [['/a', true]]);
  });
});
