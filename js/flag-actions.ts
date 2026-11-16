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

import { type FlagReply, flagFailureMessage, sendFlag } from './flag.ts';
import { type ToastKind, parseToastKind } from './toast.ts';

/** The parts of a DOM element used here, so that tests can fake them. */
export interface ElementLike {
  closest(selector: string): ElementLike | null;
  getAttribute(name: string): string | null;
}

/** What a click on a flag button should do. */
export type FlagAction = {
  url: string;
  flag: boolean;
};

/** Shows a notification, see js/notify.ts. */
export type Notify = (text: string, kind: ToastKind) => void;

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
export function flagActionFor(button: ElementLike): FlagAction | null {
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
 * Attaches click handlers to all flag buttons in the document.
 *
 * Must run once the document is parsed; module scripts (as the bundles
 * are loaded) only run after that.
 */
export function setupFlagActions(
  jquery: Pick<JQueryStatic, 'ajax'>,
  notify: Notify,
  root: ParentNode = document,
): void {
  for (const button of root.querySelectorAll('[data-flag-url] [data-flag]')) {
    const action = flagActionFor(button);
    if (action === null) {
      continue;
    }
    button.addEventListener('click', (event) => {
      event.preventDefault();
      sendFlag(jquery, action.url, action.flag)
        .done((reply) => notifyReply(notify, reply))
        .fail((xhr, status, details) =>
          notify(flagFailureMessage(xhr.status, status, details), 'danger'),
        );
    });
  }
}
