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
import { describe, it, type TestContext } from 'node:test';

import { JSDOM } from 'jsdom';
import type { Data } from 'plotly.js';

import {
  CHART_PAGES,
  type Chart,
  type CurateSeries,
  type InfoSeries,
  type NewPlot,
  type StatsSeries,
  cameraInfoCharts,
  cameraStatsCharts,
  curateCharts,
  drawCharts,
  lensInfoCharts,
  lensStatsCharts,
  showPageCharts,
} from './charts.ts';
import { plotConfig } from './plot-config.ts';

/** Distinct data, to check that each chart gets its own. */
const data = (name: string): Data[] => [{ name }];

const stats: StatsSeries = {
  imagecount: data('imagecount'),
  trends: data('trends'),
};

const info: InfoSeries = {
  ytickvals: [1.4, 2.8],
  yticktext: ['1.4', '2.8'],
  lensapfl: { lensflap: data('lensflap') },
  trends: stats,
};

// The expected layouts.

const IMAGE_COUNT_Y = {
  type: 'log',
  exponentformat: 'SI',
  rangemode: 'nonnegative',
  title: { text: 'Images' },
};

const TRENDS = {
  yaxis: { title: { text: 'Images' } },
  barmode: 'stack',
  updatemenus: [
    {
      y: 1,
      yanchor: 'bottom',
      x: 0,
      xanchor: 'left',
      buttons: [
        { method: 'relayout', args: ['barmode', 'stack'], label: 'stacked' },
        {
          method: 'relayout',
          args: ['barmode', 'group'],
          label: 'individual',
        },
      ],
    },
    {
      y: 1,
      yanchor: 'bottom',
      x: 0.1,
      xanchor: 'left',
      buttons: [
        { method: 'relayout', args: ['barnorm', ''], label: 'absolute' },
        {
          method: 'relayout',
          args: ['barnorm', 'percent'],
          label: 'normalized',
        },
      ],
    },
  ],
};

const APERTURE = {
  xaxis: { title: { text: 'Focal lengths used' } },
  yaxis: {
    title: { text: 'Aperture' },
    tickprefix: 'f/',
    tickvals: [1.4, 2.8],
    ticktext: ['1.4', '2.8'],
    type: 'log',
  },
  hovermode: 'closest',
};

/** The ids and data names of the charts. */
const summary = (charts: Chart[]) =>
  charts.map((c) => [c.id, (c.data[0] as { name: string }).name]);

describe('statistics pages', () => {
  it('shows the camera statistics', () => {
    const charts = cameraStatsCharts(stats);
    assert.deepEqual(summary(charts), [
      ['cameraChart', 'imagecount'],
      ['trendsChart', 'trends'],
    ]);
    assert.deepEqual(charts[0].layout, {
      yaxis: IMAGE_COUNT_Y,
      xaxis: { automargin: true },
    });
    assert.deepEqual(charts[1].layout, TRENDS);
  });

  it('shows the lens statistics', () => {
    const charts = lensStatsCharts(stats);
    assert.deepEqual(summary(charts), [
      ['lensChart', 'imagecount'],
      ['trendsChart', 'trends'],
    ]);
    assert.deepEqual(charts[0].layout, {
      yaxis: IMAGE_COUNT_Y,
      xaxis: { automargin: true },
    });
    assert.deepEqual(charts[1].layout, TRENDS);
  });
});

describe('information pages', () => {
  const trendsX = {
    ...TRENDS,
    xaxis: { title: { text: 'Per lens timeline stats' } },
  };

  it("shows a camera's information", () => {
    const charts = cameraInfoCharts(info);
    assert.deepEqual(summary(charts), [
      ['lensFlApChart', 'lensflap'],
      ['cameraChart', 'imagecount'],
      ['trendsChart', 'trends'],
    ]);
    assert.deepEqual(charts[0].layout, APERTURE);
    assert.deepEqual(charts[1].layout, {
      yaxis: IMAGE_COUNT_Y,
      xaxis: { automargin: true, title: { text: 'Per lens total images' } },
    });
    assert.deepEqual(charts[2].layout, trendsX);
  });

  it("shows a lens's information", () => {
    const charts = lensInfoCharts(info);
    assert.deepEqual(summary(charts), [
      ['lensFlApChart', 'lensflap'],
      ['cameraChart', 'imagecount'],
      ['trendsChart', 'trends'],
    ]);
    assert.deepEqual(charts[0].layout, APERTURE);
    assert.deepEqual(charts[1].layout, {
      yaxis: IMAGE_COUNT_Y,
      xaxis: { automargin: true, title: { text: 'Per camera total images' } },
    });
    assert.deepEqual(charts[2].layout, trendsX);
  });
});

describe('curate page', () => {
  const charts = curateCharts({
    global: data('global'),
    lenses: data('lenses'),
    folders: data('folders'),
    years: data('years'),
    months: data('months'),
  });
  const period = (name: string, extra: object) => ({
    xaxis: { title: { text: name }, automargin: true, ...extra },
    yaxis: { title: { text: 'Image count' }, rangemode: 'tozero' },
    yaxis2: {
      title: { text: 'Size' },
      overlaying: 'y',
      side: 'right',
      exponentformat: 'SI',
      rangemode: 'tozero',
    },
    legend: { orientation: 'h' },
  });

  it('shows the charts', () => {
    assert.deepEqual(summary(charts), [
      ['cameraChart', 'global'],
      ['lensChart', 'lenses'],
      ['folderChart', 'folders'],
      ['yearChart', 'years'],
      ['monthChart', 'months'],
    ]);
  });

  it('shows the file count and size charts', () => {
    assert.deepEqual(charts[0].layout, {
      xaxis: { rangemode: 'nonnegative', title: { text: 'File count' } },
      yaxis: {
        type: 'log',
        exponentformat: 'SI',
        rangemode: 'nonnegative',
        title: { text: 'Size' },
      },
      title: { text: 'Camera file/size stats' },
    });
    assert.deepEqual(charts[1].layout, {
      xaxis: { showticklabels: true, automargin: true },
      yaxis: {
        type: 'log',
        exponentformat: 'SI',
        rangemode: 'nonnegative',
        title: { text: 'File count' },
      },
      showlegend: true,
      title: { text: 'Lens image count' },
    });
    assert.deepEqual(charts[2].layout, {
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
    });
  });

  it('shows the yearly and monthly charts', () => {
    assert.deepEqual(charts[3].layout, {
      ...period('Year', {}),
      title: { text: 'Yearly image count and size' },
    });
    assert.deepEqual(charts[4].layout, {
      ...period('Month', { type: 'date' }),
      title: { text: 'Monthly image count and size' },
    });
  });
});

/** A fake Plotly.newPlot, recording the charts drawn. */
function fakePlot() {
  const drawn: Chart[] = [];
  const configs: unknown[] = [];
  const newPlot: NewPlot = (id, data, layout, config) => {
    drawn.push({ id, data, layout });
    configs.push(config);
    return Promise.resolve();
  };
  return { drawn, configs, newPlot };
}

describe('drawCharts', () => {
  it('draws each chart, with the shared configuration', () => {
    const { drawn, configs, newPlot } = fakePlot();
    const charts = cameraStatsCharts(stats);
    drawCharts(charts, newPlot);
    assert.deepEqual(drawn, charts);
    assert.deepEqual(configs, [plotConfig, plotConfig]);
  });
});

describe('showPageCharts', () => {
  /** A page with the given boot attributes. */
  const page = (attrs: string) =>
    new JSDOM(`<div id="boot" ${attrs}></div>`).window.document;

  /** A fake fetch, answering with the given value, recording the URLs. */
  function server(value: unknown) {
    const urls: string[] = [];
    const fetch = (url: string) => {
      urls.push(url);
      return Promise.resolve(new Response(JSON.stringify(value)));
    };
    return {
      urls,
      env: {
        fetch: fetch as typeof globalThis.fetch,
        pageUrl: 'https://cory.example/',
      },
    };
  }

  it('has a data URL for each page', () => {
    assert.deepEqual(
      Object.entries(CHART_PAGES).map(([name, p]) => [name, p.url]),
      [
        ['camera-stats', 'camerastats-url'],
        ['camera-info', 'camerainfo-url'],
        ['lens-stats', 'lensstats-url'],
        ['lens-info', 'lensinfo-url'],
        ['curate', 'curate-url'],
      ],
    );
  });

  const curate: CurateSeries = {
    global: data('global'),
    lenses: data('lenses'),
    folders: data('folders'),
    years: data('years'),
    months: data('months'),
  };
  // Each page, with its data and the charts expected from it.
  for (const [name, series, charts] of [
    ['camera-stats', stats, cameraStatsCharts(stats)],
    ['lens-stats', stats, lensStatsCharts(stats)],
    ['camera-info', info, cameraInfoCharts(info)],
    ['lens-info', info, lensInfoCharts(info)],
    ['curate', curate, curateCharts(curate)],
  ] as const) {
    it(`draws the ${name} charts`, async () => {
      const url = CHART_PAGES[name].url;
      const { urls, env } = server(series);
      const { drawn, newPlot } = fakePlot();
      await showPageCharts(
        newPlot,
        page(`data-init="${name}" data-${url}="/data"`),
        env,
      );
      assert.deepEqual(urls, ['/data']);
      assert.deepEqual(drawn, charts);
    });
  }

  for (const attrs of ['', 'data-init=""', 'data-init="other"']) {
    it(`does nothing for ${JSON.stringify(attrs)}`, async () => {
      const { urls, env } = server({});
      const { drawn, newPlot } = fakePlot();
      await showPageCharts(newPlot, page(attrs), env);
      assert.deepEqual(urls, []);
      assert.deepEqual(drawn, []);
    });
  }

  it('ignores inherited names', async () => {
    const { urls, env } = server({});
    const { drawn, newPlot } = fakePlot();
    await showPageCharts(newPlot, page('data-init="toString"'), env);
    assert.deepEqual(urls, []);
    assert.deepEqual(drawn, []);
  });

  it('logs a missing data URL', async (t: TestContext) => {
    const errors = t.mock.method(console, 'error', () => {}).mock;
    const { drawn, newPlot } = fakePlot();
    await showPageCharts(newPlot, page('data-init="curate"'));
    assert.deepEqual(drawn, []);
    assert.equal(errors.callCount(), 1);
  });

  it('logs failures to load the data', async (t: TestContext) => {
    const errors = t.mock.method(console, 'error', () => {}).mock;
    const { drawn, newPlot } = fakePlot();
    const fetch = (() =>
      Promise.resolve(
        new Response('', { status: 500 }),
      )) as unknown as typeof globalThis.fetch;
    await showPageCharts(
      newPlot,
      page('data-init="curate" data-curate-url="/data"'),
      { fetch, pageUrl: 'https://cory.example/' },
    );
    assert.deepEqual(drawn, []);
    assert.equal(errors.callCount(), 1);
    assert.match(String(errors.calls[0].arguments[0]), /\/data/);
  });
});
