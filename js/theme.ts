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

// The colour theme of the pages: light, dark, or automatic (following
// the system's setting). The choice is per browser, kept in its local
// storage, and applied by js/theme-boot.ts, which runs in the page's
// head, before the body is shown (so the page doesn't flash in the
// wrong theme), via Bootstrap's `data-bs-theme` attribute on <html>.
// The navbar's theme button (#theme-toggle) cycles through the choices.

import { whenReady } from './ready.ts';

/** The user's choice. */
export type ThemePref = 'auto' | 'light' | 'dark';
/** The theme actually shown. */
export type Theme = 'light' | 'dark';

/** The local storage key of the user's choice. */
export const STORAGE_KEY = 'corydalis-theme';
/** The event dispatched on the document after the shown theme changes. */
export const THEME_EVENT = 'corydalis:themechange';
/** The media query of the system's dark setting. */
export const DARK_QUERY = '(prefers-color-scheme: dark)';

/** The choices, in the order that the theme button cycles through. */
const PREFS: ThemePref[] = ['auto', 'light', 'dark'];

/** How the theme button shows each choice: its icon and description. */
export const PREF_DISPLAY: Record<ThemePref, { icon: string; label: string }> =
  {
    auto: { icon: 'fa-circle-half-stroke', label: 'automatic' },
    light: { icon: 'fa-sun', label: 'light' },
    dark: { icon: 'fa-moon', label: 'dark' },
  };

/** Parses a stored choice; anything unknown (or nothing) is 'auto'. */
export function parsePref(value: string | null | undefined): ThemePref {
  return PREFS.find((p) => p === value) ?? 'auto';
}

/** The choice after the given one, as the theme button cycles. */
export function nextPref(pref: ThemePref): ThemePref {
  return PREFS[(PREFS.indexOf(pref) + 1) % PREFS.length];
}

/** The theme shown for a choice, given the system's setting. */
export function resolveTheme(pref: ThemePref, systemDark: boolean): Theme {
  if (pref === 'auto') {
    return systemDark ? 'dark' : 'light';
  }
  return pref;
}

/**
 * Reads the stored choice. The storage can be missing or throw (e.g.
 * when disabled by the browser's privacy settings), which gives 'auto'.
 */
export function loadPref(storage: Pick<Storage, 'getItem'> | null): ThemePref {
  try {
    return parsePref(storage?.getItem(STORAGE_KEY));
  } catch {
    return 'auto';
  }
}

/**
 * Stores the choice; returns whether it worked (without storage, the
 * choice only lasts for the current page).
 */
export function savePref(
  storage: Pick<Storage, 'setItem'> | null,
  pref: ThemePref,
): boolean {
  try {
    if (storage === null) {
      return false;
    }
    storage.setItem(STORAGE_KEY, pref);
    return true;
  } catch {
    return false;
  }
}

/** The theme currently shown by the page (light unless set to dark). */
export function currentTheme(doc: Pick<Document, 'documentElement'>): Theme {
  return doc.documentElement.dataset.bsTheme === 'dark' ? 'dark' : 'light';
}

/**
 * Shows the theme for the choice: sets the page's theme, and updates
 * the theme button, if the page has one. Dispatches THEME_EVENT on the
 * document if the shown theme changed.
 */
export function applyTheme(
  doc: Document,
  pref: ThemePref,
  systemDark: boolean,
): Theme {
  const root = doc.documentElement;
  const before = root.dataset.bsTheme;
  const theme = resolveTheme(pref, systemDark);
  root.dataset.bsTheme = theme;
  root.dataset.themePref = pref;
  updateButton(doc, pref, theme);
  if (before !== undefined && before !== theme) {
    // The document's own CustomEvent, as it may come from another realm.
    const Custom = doc.defaultView?.CustomEvent ?? CustomEvent;
    doc.dispatchEvent(new Custom(THEME_EVENT, { detail: theme }));
  }
  return theme;
}

/** Shows the choice on the theme button, if the page has one. */
function updateButton(doc: Document, pref: ThemePref, theme: Theme): void {
  const button = doc.getElementById('theme-toggle');
  if (button === null) {
    return;
  }
  const next = nextPref(pref);
  const shown = pref === 'auto' ? ` (now ${theme})` : '';
  button.title =
    `Theme: ${PREF_DISPLAY[pref].label}${shown};` +
    ` click for ${PREF_DISPLAY[next].label}`;
  const icon = button.querySelector('.theme-icon');
  if (icon !== null) {
    for (const p of PREFS) {
      icon.classList.toggle(PREF_DISPLAY[p].icon, p === pref);
    }
  }
}

/** What setupTheme needs from the browser. */
export type ThemeEnv = {
  doc: Document;
  storage: Storage | null;
  media: Pick<MediaQueryList, 'matches' | 'addEventListener'> | null;
};

/**
 * Applies the stored choice right away, follows the system's setting
 * while the choice is 'auto', and, once the page is parsed, makes the
 * theme button cycle through the choices.
 */
export function setupTheme({ doc, storage, media }: ThemeEnv): void {
  let pref = loadPref(storage);
  const systemDark = () => media?.matches ?? false;
  applyTheme(doc, pref, systemDark());
  media?.addEventListener('change', () => {
    applyTheme(doc, pref, systemDark());
  });
  whenReady(() => {
    // The button was not parsed yet when the theme was first applied.
    applyTheme(doc, pref, systemDark());
    doc.getElementById('theme-toggle')?.addEventListener('click', () => {
      pref = nextPref(pref);
      savePref(storage, pref);
      applyTheme(doc, pref, systemDark());
    });
  }, doc);
}
