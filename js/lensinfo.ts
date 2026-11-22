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
import { plotConfig } from './plot-config';

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function onDataReceived(series: any) {
  const lensLayout = {
    xaxis: {
      title: { text: 'Focal lengths used' },
    },
    yaxis: {
      title: { text: 'Aperture' },
      tickprefix: 'f/',
      tickvals: series.ytickvals,
      ticktext: series.yticktext,
      type: 'log' as const,
    },
    hovermode: 'closest' as const,
  };
  const lensCameraLayout = {
    yaxis: {
      type: 'log' as const,
      exponentformat: 'SI' as const,
      rangemode: 'nonnegative' as const,
      title: { text: 'Images' },
    },
    xaxis: {
      automargin: true,
      title: { text: 'Per camera total images' },
    },
  };
  const relayout: Plotly.UpdateMenuButton['method'] = 'relayout';
  const xLeft: Plotly.UpdateMenu['xanchor'] = 'left';
  const yBottom: Plotly.UpdateMenu['yanchor'] = 'bottom';
  const trendsLayout = {
    xaxis: {
      title: { text: 'Per lens timeline stats' },
    },
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
  const config = plotConfig;
  Plotly.newPlot('lensFlApChart', series.lensapfl.lensflap, lensLayout, config);
  Plotly.newPlot(
    'cameraChart',
    series.trends.imagecount,
    lensCameraLayout,
    config,
  );
  Plotly.newPlot('trendsChart', series.trends.trends, trendsLayout, config);
}

export function initLensInfo(): void {
  void loadBootJSON('lensinfo-url', onDataReceived);
}
