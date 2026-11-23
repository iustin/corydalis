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

// The charts of the statistics pages (cameras, lenses, curate). The page
// says which charts it shows via the boot element's `data-init`
// attribute, and where to get their data via another one (e.g.
// `data-curate-url`). Plotly is passed in by js/bundle-plot.ts, so that
// the tests can run this without a browser.

import type { Config, Data, Layout, UpdateMenu } from 'plotly.js';

// With the extensions, as the unit tests run this file directly in Node.
import { bootData, loadBootJSON } from './boot.ts';
import type { RequestEnv } from './http.ts';
import { plotConfig } from './plot-config.ts';

/** A chart: the id of its element, its data and its layout. */
export type Chart = {
  id: string;
  data: Data[];
  layout: Partial<Layout>;
};

/** Draws a chart, as Plotly.newPlot. */
export type NewPlot = (
  id: string,
  data: Data[],
  layout: Partial<Layout>,
  config: Partial<Config>,
) => unknown;

/** The data of the camera and lens statistics pages. */
export type StatsSeries = {
  imagecount: Data[];
  trends: Data[];
};

/** The data of the camera and lens information pages. */
export type InfoSeries = {
  ytickvals: number[];
  yticktext: string[];
  lensapfl: { lensflap: Data[] };
  trends: StatsSeries;
};

/** The data of the curate page. */
export type CurateSeries = {
  global: Data[];
  lenses: Data[];
  folders: Data[];
  years: Data[];
  months: Data[];
};

/** The layout of the image count charts, optionally with an x axis title. */
function imageCountLayout(xTitle?: string): Partial<Layout> {
  return {
    yaxis: {
      type: 'log',
      exponentformat: 'SI',
      rangemode: 'nonnegative',
      title: { text: 'Images' },
    },
    xaxis:
      xTitle === undefined
        ? { automargin: true }
        : { automargin: true, title: { text: xTitle } },
  };
}

/** A menu (at the given horizontal position) changing a layout setting. */
function relayoutMenu(
  x: number,
  setting: string,
  choices: [label: string, value: string][],
): Partial<UpdateMenu> {
  return {
    y: 1,
    yanchor: 'bottom',
    x,
    xanchor: 'left',
    buttons: choices.map(([label, value]) => ({
      method: 'relayout',
      args: [setting, value],
      label,
    })),
  };
}

/**
 * The layout of the trends charts, optionally with an x axis title, with
 * menus to switch between stacked and individual bars, and between
 * absolute and normalized values.
 */
function trendsLayout(xTitle?: string): Partial<Layout> {
  return {
    ...(xTitle === undefined ? {} : { xaxis: { title: { text: xTitle } } }),
    yaxis: {
      title: { text: 'Images' },
    },
    barmode: 'stack', // default to stacked bars
    updatemenus: [
      relayoutMenu(0, 'barmode', [
        ['stacked', 'stack'],
        ['individual', 'group'],
      ]),
      relayoutMenu(0.1, 'barnorm', [
        ['absolute', ''],
        ['normalized', 'percent'],
      ]),
    ],
  };
}

/** The layout of the aperture by focal length charts. */
function apertureLayout(series: InfoSeries): Partial<Layout> {
  return {
    xaxis: {
      title: { text: 'Focal lengths used' },
    },
    yaxis: {
      title: { text: 'Aperture' },
      tickprefix: 'f/',
      tickvals: series.ytickvals,
      ticktext: series.yticktext,
      type: 'log',
    },
    hovermode: 'closest',
  };
}

/** The charts of the camera statistics page. */
export function cameraStatsCharts(series: StatsSeries): Chart[] {
  return [
    { id: 'cameraChart', data: series.imagecount, layout: imageCountLayout() },
    { id: 'trendsChart', data: series.trends, layout: trendsLayout() },
  ];
}

/** The charts of the lens statistics page. */
export function lensStatsCharts(series: StatsSeries): Chart[] {
  return [
    { id: 'lensChart', data: series.imagecount, layout: imageCountLayout() },
    { id: 'trendsChart', data: series.trends, layout: trendsLayout() },
  ];
}

/** The charts of a camera's page. */
export function cameraInfoCharts(series: InfoSeries): Chart[] {
  return [
    {
      id: 'lensFlApChart',
      data: series.lensapfl.lensflap,
      layout: apertureLayout(series),
    },
    {
      id: 'cameraChart',
      data: series.trends.imagecount,
      layout: imageCountLayout('Per lens total images'),
    },
    {
      id: 'trendsChart',
      data: series.trends.trends,
      layout: trendsLayout('Per lens timeline stats'),
    },
  ];
}

/** The charts of a lens's page. */
export function lensInfoCharts(series: InfoSeries): Chart[] {
  return [
    {
      id: 'lensFlApChart',
      data: series.lensapfl.lensflap,
      layout: apertureLayout(series),
    },
    {
      id: 'cameraChart',
      data: series.trends.imagecount,
      layout: imageCountLayout('Per camera total images'),
    },
    {
      id: 'trendsChart',
      data: series.trends.trends,
      layout: trendsLayout('Per lens timeline stats'),
    },
  ];
}

/** The layout of the curate page's yearly and monthly charts. */
function periodLayout(
  period: string,
  xaxis: Partial<Layout['xaxis']>,
): Partial<Layout> {
  return {
    xaxis: { title: { text: period }, automargin: true, ...xaxis },
    yaxis: {
      title: { text: 'Image count' },
      rangemode: 'tozero',
    },
    yaxis2: {
      title: { text: 'Size' },
      overlaying: 'y',
      side: 'right',
      exponentformat: 'SI',
      rangemode: 'tozero',
    },
    legend: { orientation: 'h' },
  };
}

/** The charts of the curate page. */
export function curateCharts(series: CurateSeries): Chart[] {
  return [
    {
      id: 'cameraChart',
      data: series.global,
      layout: {
        xaxis: {
          rangemode: 'nonnegative',
          title: { text: 'File count' },
        },
        yaxis: {
          type: 'log',
          exponentformat: 'SI',
          rangemode: 'nonnegative',
          title: { text: 'Size' },
        },
        title: { text: 'Camera file/size stats' },
      },
    },
    {
      id: 'lensChart',
      data: series.lenses,
      layout: {
        xaxis: {
          showticklabels: true,
          automargin: true,
        },
        yaxis: {
          type: 'log',
          exponentformat: 'SI',
          rangemode: 'nonnegative',
          title: { text: 'File count' },
        },
        showlegend: true,
        title: { text: 'Lens image count' },
      },
    },
    {
      id: 'folderChart',
      data: series.folders,
      layout: {
        xaxis: {
          type: 'log',
          exponentformat: 'SI',
          rangemode: 'nonnegative',
          title: { text: 'Size' },
        },
        yaxis: {
          type: 'log',
          rangemode: 'nonnegative',
          title: { text: 'File count' },
        },
        title: { text: 'Folder file/size stats' },
      },
    },
    {
      id: 'yearChart',
      data: series.years,
      layout: {
        ...periodLayout('Year', {}),
        title: { text: 'Yearly image count and size' },
      },
    },
    {
      id: 'monthChart',
      data: series.months,
      layout: {
        ...periodLayout('Month', { type: 'date' }),
        title: { text: 'Monthly image count and size' },
      },
    },
  ];
}

/** A page with charts: the boot data naming its data URL, and its charts. */
type ChartPage = {
  url: string;
  // The series type differs per page, hence `never` (any function fits).
  charts: (series: never) => Chart[];
};

/** The pages with charts, by their `data-init` value. */
export const CHART_PAGES: Record<string, ChartPage> = {
  'camera-stats': { url: 'camerastats-url', charts: cameraStatsCharts },
  'camera-info': { url: 'camerainfo-url', charts: cameraInfoCharts },
  'lens-stats': { url: 'lensstats-url', charts: lensStatsCharts },
  'lens-info': { url: 'lensinfo-url', charts: lensInfoCharts },
  curate: { url: 'curate-url', charts: curateCharts },
};

/** Draws the charts, with the shared configuration. */
export function drawCharts(charts: Chart[], newPlot: NewPlot): void {
  for (const chart of charts) {
    void newPlot(chart.id, chart.data, chart.layout, plotConfig);
  }
}

/**
 * Draws the charts of the page, as named by its boot data, once their
 * data is loaded. Does nothing on pages without charts. Failures to get
 * the data are logged (see loadBootJSON), but errors while drawing (e.g.
 * on unexpected data) reject the returned promise.
 */
export function showPageCharts(
  newPlot: NewPlot,
  doc: Pick<Document, 'getElementById'> = document,
  env: RequestEnv = {},
): Promise<void> {
  const name = bootData('init', doc);
  if (
    name === null ||
    !Object.prototype.hasOwnProperty.call(CHART_PAGES, name)
  ) {
    return Promise.resolve();
  }
  const page = CHART_PAGES[name];
  return loadBootJSON<never>(
    page.url,
    (series) => drawCharts(page.charts(series), newPlot),
    doc,
    env,
  );
}
