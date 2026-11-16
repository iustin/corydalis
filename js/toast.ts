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
) => Pick<Toast, 'show'>;

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

/** Returns the CSS classes of a notification. */
export function toastClasses(kind: ToastKind): string[] {
  return ['toast', 'align-items-center', 'border-0', `text-bg-${kind}`];
}

/** Whether a notification needs a light (white) close button. */
function hasDarkBackground(kind: ToastKind): boolean {
  return kind === 'success' || kind === 'danger';
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
  if (hasDarkBackground(kind)) {
    close.classList.add('btn-close-white');
  }
  close.setAttribute('data-bs-dismiss', 'toast');
  close.setAttribute('aria-label', 'Close');
  row.append(body, close);
  toast.append(row);

  // The layout always has a container; fall back to the body just in case.
  (doc.getElementById(TOAST_CONTAINER_ID) ?? doc.body).append(toast);
  // Don't accumulate hidden notifications in the page.
  toast.addEventListener('hidden.bs.toast', () => toast.remove());
  factory(toast, toastOptions(kind)).show();
}
