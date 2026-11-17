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

// The flag/un-flag buttons on the image and flagged images pages.
//
// The buttons are rendered by the server (imageFlagActions) as:
//
//   <span data-flag-url="...">
//     <button type="button" data-flag="true">Flag</button>
//     <button type="button" data-flag="false">Un-flag</button>
//   </span>
//
// Clicking a button sends a PUT (flag) or DELETE (un-flag) request to the
// URL, and shows the server's reply (or the error) as a notification.
//
// The click handlers must be attached here: the content security policy
// forbids inline handlers (onclick attributes) in the HTML.

import { type FlagReply, flagErrorMessage, sendFlag } from './flag.ts';
import { type ToastKind, parseToastKind } from './toast.ts';

/** What a click on a flag button should do. */
export type FlagAction = {
  url: string;
  flag: boolean;
};

/** Shows a notification, see js/notify.ts. */
export type Notify = (text: string, kind: ToastKind) => void;

/** Sends a flag request, see sendFlag; replaceable in the tests. */
export type SendFlag = (url: string, flag: boolean) => Promise<FlagReply>;

/** Parses the value of a button's `data-flag` attribute. */
export function parseFlagValue(value: string | null): boolean | null {
  switch (value) {
    case 'true':
      return true;
    case 'false':
      return false;
    default:
      return null;
  }
}

/**
 * Returns the action of a flag button, or null if the button is not
 * part of a (well-formed) flag widget.
 */
export function flagActionFor(button: Element): FlagAction | null {
  const container = button.closest('[data-flag-url]');
  if (container === null) {
    return null;
  }
  const url = container.getAttribute('data-flag-url');
  const flag = parseFlagValue(button.getAttribute('data-flag'));
  if (url === null || url === '' || flag === null) {
    return null;
  }
  return { url, flag };
}

/** Shows the server's reply to a flag request. */
export function notifyReply(notify: Notify, reply: FlagReply): void {
  notify(reply.text, parseToastKind(reply.kind));
}

/**
 * Sends the request of a flag action, and shows its result: the server's
 * reply, or the failure as an error. Never fails.
 */
export function runFlagAction(
  notify: Notify,
  action: FlagAction,
  send: SendFlag = sendFlag,
): Promise<void> {
  return send(action.url, action.flag).then(
    (reply) => notifyReply(notify, reply),
    (err: unknown) => notify(flagErrorMessage(err), 'danger'),
  );
}

/**
 * Attaches click handlers to all flag buttons in the document.
 *
 * Must run once the document is parsed; module scripts (as the bundles
 * are loaded) only run after that.
 */
export function setupFlagActions(
  notify: Notify,
  root: ParentNode = document,
  send: SendFlag = sendFlag,
): void {
  for (const button of root.querySelectorAll('[data-flag-url] [data-flag]')) {
    const action = flagActionFor(button);
    if (action === null) {
      continue;
    }
    button.addEventListener('click', (event) => {
      event.preventDefault();
      void runFlagAction(notify, action, send);
    });
  }
}
