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

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function onDataReceived(series: any) {
  const cameraLayout = {
    xaxis: {
      rangemode: 'nonnegative' as const,
      title: { text: 'File count' },
    },
    yaxis: {
      type: 'log' as const,
      exponentformat: 'SI' as const,
      rangemode: 'nonnegative' as const,
      title: { text: 'Size' },
    },
    title: { text: 'Camera file/size stats' },
  };
  const lensLayout = {
    xaxis: {
      showticklabels: true,
      automargin: true,
    },
    yaxis: {
      type: 'log' as const,
      exponentformat: 'SI' as const,
      rangemode: 'nonnegative' as const,
      title: { text: 'File count' },
    },
    showlegend: true,
    title: { text: 'Lens image count' },
  };
  const folderLayout = {
    xaxis: {
      type: 'log' as const,
      exponentformat: 'SI' as const,
      rangemode: 'nonnegative' as const,
      title: { text: 'Size' },
    },
    yaxis: {
      type: 'log' as const,
      rangemode: 'nonnegative' as const,
      title: { text: 'File count' },
    },
    title: { text: 'Folder file/size stats' },
  };
  const yearLayout = {
    xaxis: {
      title: { text: 'Year' },
      automargin: true,
    },
    yaxis: {
      title: { text: 'Image count' },
      rangemode: 'tozero' as const,
    },
    yaxis2: {
      title: { text: 'Size' },
      overlaying: 'y' as const,
      side: 'right' as const,
      exponentformat: 'SI' as const,
      rangemode: 'tozero' as const,
    },
    legend: { orientation: 'h' as const },
    title: { text: 'Yearly image count and size' },
  };
  const monthLayout = {
    xaxis: {
      title: { text: 'Month' },
      automargin: true,
      type: 'date' as const,
    },
    yaxis: {
      title: { text: 'Image count' },
      rangemode: 'tozero' as const,
    },
    yaxis2: {
      title: { text: 'Size' },
      overlaying: 'y' as const,
      side: 'right' as const,
      exponentformat: 'SI' as const,
      rangemode: 'tozero' as const,
    },
    legend: { orientation: 'h' as const },
    title: { text: 'Monthly image count and size' },
  };
  const config = {
    showLink: false,
    sendData: false,
    displaylogo: false,
    // Never render text as LaTeX (which would need MathJax): the
    // charts show names from the pictures' metadata.
    typesetMath: false,
    modeBarButtonsToRemove: ['toImage' as const, 'sendChartToCloud' as const],
    responsive: true,
  };
  Plotly.newPlot('cameraChart', series.global, cameraLayout, config);
  Plotly.newPlot('lensChart', series.lenses, lensLayout, config);
  Plotly.newPlot('folderChart', series.folders, folderLayout, config);
  Plotly.newPlot('yearChart', series.years, yearLayout, config);
  Plotly.newPlot('monthChart', series.months, monthLayout, config);
}

export function initCurate(): void {
  void loadBootJSON('curate-url', onDataReceived);
}
