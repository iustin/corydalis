// Shows notifications as Bootstrap toasts, connecting js/toast.ts to
// Bootstrap. Kept separate, as Bootstrap can only be loaded in a browser.

import { Toast } from 'bootstrap';
import { type ToastKind, showToast } from './toast';

/** Shows a transient notification. */
export function notify(text: string, kind: ToastKind): void {
  showToast((element, options) => new Toast(element, options), text, kind);
}
