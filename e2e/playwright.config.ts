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

// Playwright Test configuration for the e2e tests (see README.md). The
// servers are started by run.sh, which sets the environment variables
// below; the "private" project uses the normal server and library, the
// "public" one the public-site server and the library with a problem
// picture. Both libraries have a movie (see add-movie.sh), so the
// movie tests run in both.

import { defineConfig } from '@playwright/test';

const out = process.env.E2E_OUT ?? 'test-results';

export default defineConfig({
  testDir: './tests',
  outputDir: `${out}/test-results`,
  // Some tests change the server's state (flagging), so run them one at
  // a time; the whole run takes about a minute anyway.
  workers: 1,
  fullyParallel: false,
  // No retries: a test that passes only on a retry hides a real
  // problem (in the code, or in the test).
  retries: 0,
  reporter: [
    ['list'],
    ['html', { outputFolder: `${out}/report`, open: 'never' }],
  ],
  use: {
    viewport: { width: 1280, height: 900 },
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
  },
  projects: [
    {
      name: 'private',
      testIgnore: /public-site/,
      use: { baseURL: process.env.CORYDALIS_URL ?? 'http://localhost:3001' },
    },
    {
      name: 'public',
      testMatch: /public-site|movie/,
      use: {
        baseURL: process.env.CORYDALIS_PUBLIC_URL ?? 'http://localhost:3002',
      },
    },
  ],
});
