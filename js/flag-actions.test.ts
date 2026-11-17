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
  setupFlagActions,
} from './flag-actions.ts';
import { type FlagReply, flagFailureMessage } from './flag.ts';

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
    const { shown } = run({
      reply: { text: 'Image flagged', kind: 'success' },
    });
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

describe('setupFlagActions', () => {
  /** A jQuery whose requests are recorded, and never answered. */
  function recordingJquery() {
    const requests: JQuery.AjaxSettings[] = [];
    const xhr = { done: () => xhr, fail: () => xhr };
    const jquery = {
      ajax: (settings: JQuery.AjaxSettings) => {
        requests.push(settings);
        return xhr;
      },
    } as unknown as Pick<JQueryStatic, 'ajax'>;
    return { jquery, requests };
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
    const { jquery, requests } = recordingJquery();
    setupFlagActions(jquery, () => {}, doc);
    assert.equal(click(flagButton), true, 'prevents the default action');
    click(unflagButton);
    assert.deepEqual(
      requests.map((r) => [r.url, r.type]),
      [
        ['/image/flag/a/b', 'PUT'],
        ['/image/flag/a/b', 'DELETE'],
      ],
    );
  });

  it('leaves malformed widgets alone', () => {
    const { doc, flagButton } = widget('');
    const { jquery, requests } = recordingJquery();
    setupFlagActions(jquery, () => {}, doc);
    assert.equal(click(flagButton), false);
    assert.deepEqual(requests, []);
  });

  it('only looks inside the given root', () => {
    const doc = page(`<div id="a">${WIDGET('/a')}</div>${WIDGET('/b')}`);
    const { jquery, requests } = recordingJquery();
    setupFlagActions(jquery, () => {}, doc.getElementById('a')!);
    for (const button of doc.querySelectorAll('[data-flag="true"]')) {
      click(button);
    }
    assert.deepEqual(
      requests.map((r) => r.url),
      ['/a'],
    );
  });
});
