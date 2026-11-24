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

// PagePlot bundle: the statistics pages, with charts (see js/charts.ts)
// and sortable tables.
import * as Plotly from 'plotly.js-cartesian-dist';
import 'bootstrap';
import { pageTheming, showPageCharts } from './charts';
import { whenReady } from './ready';
import { setupSortableTables } from './tablesort';

whenReady(() => {
  void showPageCharts(Plotly.newPlot, document, {}, pageTheming());
});

setupSortableTables();
