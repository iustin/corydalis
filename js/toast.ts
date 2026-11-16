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

// Transient notifications ("toasts"), shown in the page's toast container.
//
// This module doesn't import Bootstrap itself (which needs a browser to
// load), so that it can be unit-tested; the Bootstrap Toast is passed in.

import type { Toast } from 'bootstrap';

/** The kinds of notifications, matching the server's message kinds. */
export type ToastKind = 'success' | 'info' | 'warning' | 'danger';

const TOAST_KINDS: readonly ToastKind[] = [
  'success',
  'info',
  'warning',
  'danger',
];

/** How long notifications stay visible, in milliseconds. */
export const TOAST_DELAY = 3000;

/** The id of the toast container in the page layout. */
export const TOAST_CONTAINER_ID = 'toast-container';

/** Creates and returns a Bootstrap toast for an element. */
export type ToastFactory = (
  element: Element,
  options: Partial<Toast.Options>,
) => Pick<Toast, 'show' | 'dispose'>;

/**
 * Parses a notification kind (e.g. from a server reply), defaulting to
 * 'info' like the server does for unknown kinds.
 */
export function parseToastKind(value: unknown): ToastKind {
  return TOAST_KINDS.find((kind) => kind === value) ?? 'info';
}

/**
 * Returns the Bootstrap options for a notification. Errors stay until
 * closed, so that they're not missed.
 */
export function toastOptions(kind: ToastKind): Partial<Toast.Options> {
  return kind === 'danger'
    ? { autohide: false }
    : { autohide: true, delay: TOAST_DELAY };
}

/** How a kind of notification looks. */
export type ToastStyle = {
  /** Bootstrap classes for the colours (and border) of the notification. */
  classes: readonly string[];
  /** Whether the close button must be light, for dark backgrounds. */
  lightCloseButton: boolean;
};

/**
 * The presentation of each kind of notification. Change these to restyle
 * notifications; e.g. a softer error style would be:
 *
 *   classes: ['bg-danger-subtle', 'text-danger-emphasis', 'border',
 *             'border-danger-subtle'],
 *   lightCloseButton: false,
 */
export const TOAST_STYLES: Readonly<Record<ToastKind, ToastStyle>> = {
  success: {
    classes: ['bg-success-subtle', 'text-success-emphasis', 'border-0'],
    lightCloseButton: true,
  },
  info: {
    classes: ['bg-info-subtle', 'text-info-emphasis', 'border-0'],
    lightCloseButton: false,
  },
  warning: {
    classes: ['bg-secondary-subtle', 'text-secondary-emphasis', 'border-0'],
    lightCloseButton: false,
  },
  danger: {
    classes: ['bg-danger-subtle', 'text-danger-emphasis', 'border-0'],
    lightCloseButton: true,
  },
};

/** Returns the CSS classes of a notification. */
export function toastClasses(kind: ToastKind): string[] {
  return ['toast', 'align-items-center', ...TOAST_STYLES[kind].classes];
}

/** Shows a notification with the given text. */
export function showToast(
  factory: ToastFactory,
  text: string,
  kind: ToastKind,
  doc: Document = document,
): void {
  const toast = doc.createElement('div');
  toast.classList.add(...toastClasses(kind));
  // Errors interrupt screen readers, other notifications wait.
  toast.setAttribute('role', kind === 'danger' ? 'alert' : 'status');
  toast.setAttribute('aria-live', kind === 'danger' ? 'assertive' : 'polite');
  toast.setAttribute('aria-atomic', 'true');

  const row = doc.createElement('div');
  row.classList.add('d-flex');
  const body = doc.createElement('div');
  body.classList.add('toast-body');
  // Text only: messages may contain details from the server.
  body.textContent = text;
  const close = doc.createElement('button');
  close.type = 'button';
  close.classList.add('btn-close', 'me-2', 'm-auto');
  if (TOAST_STYLES[kind].lightCloseButton) {
    close.classList.add('btn-close-white');
  }
  close.setAttribute('data-bs-dismiss', 'toast');
  close.setAttribute('aria-label', 'Close');
  row.append(body, close);
  toast.append(row);

  // The layout always has a container; fall back to the body just in case.
  (doc.getElementById(TOAST_CONTAINER_ID) ?? doc.body).append(toast);
  const instance = factory(toast, toastOptions(kind));
  removeWhenHidden(toast, instance);
  instance.show();
}

/** The parts of a toast element used by removeWhenHidden. */
export type RemovableElement = Pick<EventTarget, 'addEventListener'> & {
  remove(): void;
};

/**
 * Removes a notification once it's hidden (by its timer or the close
 * button), so that hidden notifications don't accumulate in the page.
 * Bootstrap only hides the element, and keeps the Toast instance in its
 * own registry until it's disposed of.
 */
export function removeWhenHidden(
  element: RemovableElement,
  instance: Pick<Toast, 'dispose'>,
): void {
  element.addEventListener(
    'hidden.bs.toast',
    () => {
      instance.dispose();
      element.remove();
    },
    { once: true },
  );
}
