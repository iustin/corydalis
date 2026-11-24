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

import { STORAGE_KEY } from './theme.ts';

describe('theme entry point', () => {
  it('sets the stored theme on the page', async () => {
    const { window } = new JSDOM('<html><body></body></html>', {
      url: 'https://cory.example/',
    });
    window.localStorage.setItem(STORAGE_KEY, 'dark');
    const g = globalThis as { window?: unknown };
    g.window = window;
    try {
      await import('./theme-boot.ts');
    } finally {
      delete g.window;
    }
    assert.equal(window.document.documentElement.dataset.bsTheme, 'dark');
  });
});
