// The flag/un-flag buttons on the image and flagged images pages.
//
// The buttons are rendered by the server (imageFlagActions) as:
//
//   <span data-flag-url="...">
//     <button type="button" data-flag="true">Flag</button>
//     <button type="button" data-flag="false">Un-flag</button>
//     <span class="flag-status" role="status"></span>
//   </span>
//
// Clicking a button sends a PUT (flag) or DELETE (un-flag) request to the
// URL, and shows the server's reply in the status element.
//
// The click handlers must be attached here: the content security policy
// forbids inline handlers (onclick attributes) in the HTML.

import { flagFailureMessage, sendFlag } from './flag.ts';

/** The parts of a DOM element used here, so that tests can fake them. */
export interface ElementLike {
  closest(selector: string): ElementLike | null;
  getAttribute(name: string): string | null;
  querySelector(selector: string): ElementLike | null;
}

/** What a click on a flag button should do. */
export type FlagAction<E extends ElementLike> = {
  url: string;
  flag: boolean;
  /** Where to show the result, if the page has a place for it. */
  status: E | null;
};

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
export function flagActionFor<E extends ElementLike>(
  button: E,
): FlagAction<E> | null {
  const container = button.closest('[data-flag-url]');
  if (container === null) {
    return null;
  }
  const url = container.getAttribute('data-flag-url');
  const flag = parseFlagValue(button.getAttribute('data-flag'));
  if (url === null || url === '' || flag === null) {
    return null;
  }
  return {
    url,
    flag,
    status: container.querySelector('.flag-status') as E | null,
  };
}

/** Shows a message in a status element. */
function showStatus(status: Element | null, text: string, isError: boolean) {
  if (status === null) {
    return;
  }
  // Text only: messages may contain details from the server.
  status.textContent = text;
  status.classList.toggle('text-danger', isError);
}

/**
 * Attaches click handlers to all flag buttons in the document.
 *
 * Must run once the document is parsed; module scripts (as the bundles
 * are loaded) only run after that.
 */
export function setupFlagActions(
  jquery: Pick<JQueryStatic, 'ajax'>,
  root: ParentNode = document,
): void {
  for (const button of root.querySelectorAll('[data-flag-url] [data-flag]')) {
    const action = flagActionFor(button);
    if (action === null) {
      continue;
    }
    button.addEventListener('click', (event) => {
      event.preventDefault();
      showStatus(action.status, '…', false);
      sendFlag(jquery, action.url, action.flag)
        .done((json) => showStatus(action.status, json.text, false))
        .fail((xhr, status, details) =>
          showStatus(
            action.status,
            flagFailureMessage(xhr.status, status, details),
            true,
          ),
        );
    });
  }
}
