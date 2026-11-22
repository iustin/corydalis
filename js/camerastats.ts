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

import * as Plotly from 'plotly.js-cartesian-dist';
import 'bootstrap';
import { loadBootJSON } from './boot';
import { escapeTraces } from './plot-text';

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function onDataReceived(series: any) {
  const cameraLayout = {
    yaxis: {
      type: 'log' as const,
      exponentformat: 'SI' as const,
      rangemode: 'nonnegative' as const,
      title: { text: 'Images' },
    },
    xaxis: {
      automargin: true,
    },
  };
  const relayout: Plotly.UpdateMenuButton['method'] = 'relayout';
  const xLeft: Plotly.UpdateMenu['xanchor'] = 'left';
  const yBottom: Plotly.UpdateMenu['yanchor'] = 'bottom';
  const trendsLayout = {
    yaxis: {
      title: { text: 'Images' },
    },
    barmode: 'stack' as const, // default to stacked bars
    updatemenus: [
      {
        y: 1,
        yanchor: yBottom,
        x: 0,
        xanchor: xLeft,
        buttons: [
          {
            method: relayout,
            args: ['barmode', 'stack'],
            label: 'stacked',
          },
          {
            method: relayout,
            args: ['barmode', 'group'],
            label: 'individual',
          },
        ],
      },
      {
        y: 1,
        yanchor: yBottom,
        x: 0.1,
        xanchor: xLeft,
        buttons: [
          {
            method: relayout,
            args: ['barnorm', ''],
            label: 'absolute',
          },
          {
            method: relayout,
            args: ['barnorm', 'percent'],
            label: 'normalized',
          },
        ],
      },
    ],
  };
  const config = {
    showLink: false,
    sendData: false,
    displaylogo: false,
    modeBarButtonsToRemove: ['toImage' as const, 'sendChartToCloud' as const],
    responsive: true,
  };
  Plotly.newPlot('cameraChart', escapeTraces(series.imagecount), cameraLayout, config);
  Plotly.newPlot('trendsChart', escapeTraces(series.trends), trendsLayout, config);
}

export function initCameraStats(): void {
  void loadBootJSON('camerastats-url', onDataReceived);
}
