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

import type { Config } from 'plotly.js';

/** The Plotly configuration shared by all the charts. */
export const plotConfig: Partial<Config> = {
  showLink: false,
  sendData: false,
  displaylogo: false,
  // Never render text as LaTeX (which would need MathJax): the charts
  // show names from the pictures' metadata.
  typesetMath: false,
  modeBarButtonsToRemove: ['toImage', 'sendChartToCloud'],
  responsive: true,
};
