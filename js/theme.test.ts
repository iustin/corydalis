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
  STORAGE_KEY,
  THEME_EVENT,
  applyTheme,
  browserThemeEnv,
  currentTheme,
  loadPref,
  nextPref,
  parsePref,
  resolveTheme,
  savePref,
  setupTheme,
} from './theme.ts';

/** A page with the navbar's theme button, or without any. */
function page(withButton = true): Document {
  const button = withButton
    ? '<button id="theme-toggle"><span class="fa-solid fa-circle-half-stroke theme-icon"></span></button>'
    : '';
  return new JSDOM(`<html><body>${button}</body></html>`).window.document;
}

/** A storage keeping its values in a map. */
function memoryStorage(initial: Record<string, string> = {}) {
  const values = new Map(Object.entries(initial));
  return {
    values,
    getItem: (key: string) => values.get(key) ?? null,
    setItem: (key: string, value: string) => {
      values.set(key, value);
    },
  } as unknown as Storage & { values: Map<string, string> };
}

/** A storage that refuses all access, as with disabled site data. */
const brokenStorage = {
  getItem: () => {
    throw new Error('denied');
  },
  setItem: () => {
    throw new Error('denied');
  },
} as unknown as Storage;

/** The system's dark setting, which the test can change. */
function fakeMedia(matches: boolean) {
  const target = new EventTarget();
  return {
    matches,
    addEventListener: target.addEventListener.bind(target),
    set(value: boolean) {
      this.matches = value;
      target.dispatchEvent(new Event('change'));
    },
  };
}

/** The icon classes of the theme button. */
function iconOf(doc: Document): string {
  return doc.querySelector('.theme-icon')!.className;
}

describe('theme choices', () => {
  it('parses the stored choices, defaulting to auto', () => {
    assert.equal(parsePref('light'), 'light');
    assert.equal(parsePref('dark'), 'dark');
    assert.equal(parsePref('auto'), 'auto');
    assert.equal(parsePref(null), 'auto');
    assert.equal(parsePref(undefined), 'auto');
    assert.equal(parsePref('purple'), 'auto');
    assert.equal(parsePref('toString'), 'auto');
  });

  it('cycles through auto, light and dark', () => {
    assert.equal(nextPref('auto'), 'light');
    assert.equal(nextPref('light'), 'dark');
    assert.equal(nextPref('dark'), 'auto');
  });

  it('follows the system only for auto', () => {
    assert.equal(resolveTheme('auto', false), 'light');
    assert.equal(resolveTheme('auto', true), 'dark');
    assert.equal(resolveTheme('light', true), 'light');
    assert.equal(resolveTheme('dark', false), 'dark');
  });
});

describe('theme storage', () => {
  it('loads and saves the choice', () => {
    const storage = memoryStorage();
    assert.equal(loadPref(storage), 'auto');
    assert.equal(savePref(storage, 'dark'), true);
    assert.equal(storage.values.get(STORAGE_KEY), 'dark');
    assert.equal(loadPref(storage), 'dark');
  });

  it('works without storage, or with a failing one', () => {
    assert.equal(loadPref(null), 'auto');
    assert.equal(savePref(null, 'dark'), false);
    assert.equal(loadPref(brokenStorage), 'auto');
    assert.equal(savePref(brokenStorage, 'dark'), false);
  });
});

describe('applyTheme', () => {
  it('sets the theme on the root element', () => {
    const doc = page();
    assert.equal(currentTheme(doc), 'light');
    assert.equal(applyTheme(doc, 'dark', false), 'dark');
    assert.equal(doc.documentElement.dataset.bsTheme, 'dark');
    assert.equal(doc.documentElement.dataset.themePref, 'dark');
    assert.equal(currentTheme(doc), 'dark');
    applyTheme(doc, 'auto', false);
    assert.equal(currentTheme(doc), 'light');
  });

  it('shows the choice on the button', () => {
    const doc = page();
    const button = doc.getElementById('theme-toggle')!;
    applyTheme(doc, 'auto', true);
    assert.equal(button.title, 'Theme: automatic (now dark); click for light');
    assert.match(iconOf(doc), /\bfa-circle-half-stroke\b/);
    applyTheme(doc, 'light', true);
    assert.equal(button.title, 'Theme: light; click for dark');
    assert.match(iconOf(doc), /\bfa-sun\b/);
    assert.doesNotMatch(iconOf(doc), /fa-circle-half-stroke|fa-moon/);
    applyTheme(doc, 'dark', true);
    assert.equal(button.title, 'Theme: dark; click for automatic');
    assert.match(iconOf(doc), /\bfa-moon\b/);
    assert.doesNotMatch(iconOf(doc), /fa-circle-half-stroke|fa-sun/);
    assert.match(iconOf(doc), /\btheme-icon\b/);
  });

  it('works on pages without the button', () => {
    const doc = page(false);
    assert.equal(applyTheme(doc, 'dark', false), 'dark');
  });

  it('announces changes of the shown theme only', () => {
    const doc = page();
    const events: unknown[] = [];
    doc.addEventListener(THEME_EVENT, (e) =>
      events.push((e as CustomEvent).detail),
    );
    applyTheme(doc, 'light', false); // the initial theme
    applyTheme(doc, 'auto', false); // still light
    applyTheme(doc, 'dark', false);
    applyTheme(doc, 'dark', false);
    applyTheme(doc, 'light', false);
    assert.deepEqual(events, ['dark', 'light']);
  });
});

describe('setupTheme', () => {
  it('applies the stored choice right away', () => {
    const doc = page();
    setupTheme({
      doc,
      storage: memoryStorage({ [STORAGE_KEY]: 'dark' }),
      media: fakeMedia(false),
    });
    assert.equal(currentTheme(doc), 'dark');
  });

  it('follows the system while on auto', () => {
    const doc = page();
    const media = fakeMedia(false);
    setupTheme({ doc, storage: memoryStorage(), media });
    assert.equal(currentTheme(doc), 'light');
    media.set(true);
    assert.equal(currentTheme(doc), 'dark');
    media.set(false);
    assert.equal(currentTheme(doc), 'light');
  });

  it('ignores the system for an explicit choice', () => {
    const doc = page();
    const media = fakeMedia(false);
    setupTheme({
      doc,
      storage: memoryStorage({ [STORAGE_KEY]: 'light' }),
      media,
    });
    media.set(true);
    assert.equal(currentTheme(doc), 'light');
  });

  it('cycles and stores the choice on clicks', () => {
    const doc = page();
    const storage = memoryStorage();
    setupTheme({ doc, storage, media: fakeMedia(true) });
    const button = doc.getElementById('theme-toggle')!;
    assert.equal(currentTheme(doc), 'dark'); // auto, with a dark system
    button.click();
    assert.equal(storage.values.get(STORAGE_KEY), 'light');
    assert.equal(currentTheme(doc), 'light');
    button.click();
    assert.equal(storage.values.get(STORAGE_KEY), 'dark');
    assert.equal(currentTheme(doc), 'dark');
    button.click();
    assert.equal(storage.values.get(STORAGE_KEY), 'auto');
    assert.equal(currentTheme(doc), 'dark');
  });

  it('works without storage nor media queries', () => {
    const doc = page();
    setupTheme({ doc, storage: null, media: null });
    assert.equal(currentTheme(doc), 'light');
    doc.getElementById('theme-toggle')!.click();
    assert.equal(currentTheme(doc), 'light');
    doc.getElementById('theme-toggle')!.click();
    assert.equal(currentTheme(doc), 'dark');
  });

  it('sets up the button once the page is parsed', () => {
    // The script runs in the head, before the body is parsed.
    const { window } = new JSDOM('<html><head></head>', {
      runScripts: 'outside-only',
    });
    const doc = window.document;
    Object.defineProperty(doc, 'readyState', {
      value: 'loading',
      configurable: true,
    });
    setupTheme({
      doc,
      storage: memoryStorage({ [STORAGE_KEY]: 'dark' }),
      media: fakeMedia(false),
    });
    // Before the body is shown, so that it doesn't flash in light.
    assert.equal(currentTheme(doc), 'dark');
    doc.body.innerHTML =
      '<button id="theme-toggle"><span class="fa-solid theme-icon"></span></button>';
    doc.dispatchEvent(new window.Event('DOMContentLoaded'));
    const button = doc.getElementById('theme-toggle')!;
    assert.match(button.title, /^Theme: dark;/);
    assert.match(iconOf(doc), /\bfa-moon\b/);
    button.click();
    assert.equal(currentTheme(doc), 'light'); // auto, with a light system
    assert.match(iconOf(doc), /\bfa-circle-half-stroke\b/);
    button.click();
    assert.equal(currentTheme(doc), 'light');
  });

  it('ignores the system once a choice is clicked', () => {
    const doc = page();
    const media = fakeMedia(false);
    setupTheme({ doc, storage: memoryStorage(), media });
    doc.getElementById('theme-toggle')!.click(); // now light
    media.set(true);
    assert.equal(currentTheme(doc), 'light');
  });
});

describe('browserThemeEnv', () => {
  it("uses the window's document, storage and media query", () => {
    const doc = page();
    const storage = memoryStorage();
    const media = fakeMedia(true);
    const queries: string[] = [];
    const env = browserThemeEnv({
      document: doc,
      localStorage: storage,
      matchMedia: ((query: string) => {
        queries.push(query);
        return media;
      }) as unknown as Window['matchMedia'],
    });
    assert.equal(env.doc, doc);
    assert.equal(env.storage, storage);
    assert.equal(env.media, media);
    assert.deepEqual(queries, ['(prefers-color-scheme: dark)']);
  });

  it('works when the storage is refused, without media queries', () => {
    const doc = page();
    const win = {
      document: doc,
      get localStorage(): Storage {
        throw new Error('denied');
      },
    };
    const env = browserThemeEnv(win);
    assert.equal(env.storage, null);
    assert.equal(env.media, null);
    setupTheme(env);
    assert.equal(currentTheme(doc), 'light');
  });
});
