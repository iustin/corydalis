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

// Records the state of every chart on the plot pages (the camera and
// lens statistics, the curate page, and one camera and one lens page)
// to OUTDIR/report.json, with screenshots: the traces, bars and points,
// legend and axis labels, hover labels, the result of the update menus
// (stacked/individual, absolute/normalized), and the width after
// resizing the window; plus the plotly version that the browser
// actually received, and any console errors. Useful to compare plotly
// versions (or other changes to the plots): run it before and after,
// then compare the two reports; see README.md.
//
// Usage: node e2e/plots.cjs OUTDIR
//        node e2e/plots.cjs --compare OUTDIR1 OUTDIR2

const fs = require('fs');
const path = require('path');

/** Returns the state of a chart, from plotly's data and the SVG. */
async function chartState(page, id) {
  return page.evaluate((id) => {
    const gd = document.getElementById(id);
    const fl = gd._fullLayout;
    return {
      width: fl.width,
      height: fl.height,
      barmode: fl.barmode,
      barnorm: fl.barnorm,
      traces: gd._fullData.map((t) => ({
        type: t.type,
        name: t.name,
        visible: t.visible,
        n: (t.x || t.labels || []).length,
      })),
      bars: gd.querySelectorAll('.bars .point path').length,
      points: gd.querySelectorAll('.scatterlayer .point').length,
      legend: [...gd.querySelectorAll('.legendtext')].map((e) => e.textContent),
      xticks: [...gd.querySelectorAll('.xtick text')].map((e) => e.textContent),
      menus: [...gd.querySelectorAll('.updatemenu-header-group text')].map(
        (e) => e.textContent,
      ),
    };
  }, id);
}

/** Waits until all the given charts are drawn. */
async function waitForCharts(page, charts) {
  for (const c of charts) {
    await page.locator(`#${c} .main-svg`).first().waitFor({ timeout: 10000 });
  }
}

async function record(out) {
  const { chromium } = require('playwright');
  const { BASE, collectErrors, login } = require('./lib.cjs');
  const browser = await chromium.launch();
  const page = await browser.newPage({
    viewport: { width: 1400, height: 1000 },
  });
  const errors = collectErrors(page);
  // The plot bundles that the browser actually received.
  const bundles = new Set();
  page.on('response', (resp) => {
    if (/bundle-plot\.js/.test(resp.url())) {
      bundles.add(
        `${resp.status()} ${new URL(resp.url()).pathname}${new URL(resp.url()).search}`,
      );
    }
  });
  await login(page);

  const pages = [
    ['cameras', '/stats/cameras', ['cameraChart', 'trendsChart']],
    ['lenses', '/stats/lenses', ['lensChart', 'trendsChart']],
    [
      'curate',
      '/curate',
      ['cameraChart', 'lensChart', 'folderChart', 'yearChart', 'monthChart'],
    ],
  ];
  // The first camera and lens pages, with all their charts.
  for (const [name, list, link] of [
    ['camera', '/stats/cameras', 'a[href*="/stats/camera/"]'],
    ['lens', '/stats/lenses', 'a[href*="/stats/lens/"]'],
  ]) {
    await page.goto(BASE + list);
    const href = await page.locator(link).first().getAttribute('href');
    const url = new URL(href, BASE);
    await page.goto(url.href);
    await page.locator('.js-plotly-plot').first().waitFor({ timeout: 10000 });
    const ids = await page.$$eval('.js-plotly-plot', (es) =>
      es.map((e) => e.id),
    );
    pages.push([name, url.pathname, ids]);
  }

  const report = { pages: {} };
  for (const [name, url, charts] of pages) {
    await page.setViewportSize({ width: 1400, height: 1000 });
    await page.goto(BASE + url);
    await waitForCharts(page, charts);
    const r = { url, charts: {} };
    for (const c of charts)
      r.charts[c] = { initial: await chartState(page, c) };
    report.plotly = await page.evaluate(
      () => (window.Plotly && window.Plotly.version) || null,
    );
    await page.screenshot({
      path: path.join(out, `${name}.png`),
      fullPage: true,
    });

    // Hover over the first bar or point of each chart.
    for (const c of charts) {
      const target = page
        .locator(`#${c} .bars .point path, #${c} .scatterlayer .point`)
        .first();
      if (await target.count()) {
        await target.hover({ force: true });
        await page
          .locator(`#${c} .hoverlayer .hovertext`)
          .first()
          .waitFor({ timeout: 2000 })
          .catch(() => {});
        r.charts[c].hover = await page
          .locator(`#${c} .hoverlayer .hovertext`)
          .count();
      }
    }

    // Select the second button of each update menu, in turn.
    for (const c of charts) {
      const headers = page.locator(`#${c} .updatemenu-header-group`);
      const n = await headers.count();
      if (!n) continue;
      r.charts[c].menus = [];
      for (let i = 0; i < n; i++) {
        await headers.nth(i).click();
        const buttons = page.locator(`#${c} .updatemenu-dropdown-button`);
        await buttons.first().waitFor({ timeout: 2000 });
        if ((await buttons.count()) > 1) {
          const layout = (id) => {
            const fl = document.getElementById(id)._fullLayout;
            return `${fl.barmode}/${fl.barnorm}`;
          };
          const before = await page.evaluate(layout, c);
          await buttons.nth(1).click();
          // Wait for the relayout (the bar mode or normalisation changes).
          await page
            .waitForFunction(
              ([id, prev]) => {
                const fl = document.getElementById(id)._fullLayout;
                return `${fl.barmode}/${fl.barnorm}` !== prev;
              },
              [c, before],
              { timeout: 5000 },
            )
            .catch(() => {});
        }
        const s = await chartState(page, c);
        r.charts[c].menus.push({
          barmode: s.barmode,
          barnorm: s.barnorm,
          bars: s.bars,
        });
      }
    }
    if (charts.some((c) => r.charts[c].menus)) {
      await page.screenshot({
        path: path.join(out, `${name}-menus.png`),
        fullPage: true,
      });
    }

    // Responsive resizing (the charts use responsive: true).
    await page.goto(BASE + url);
    await waitForCharts(page, charts);
    const first = charts[0];
    const wide = (await chartState(page, first)).width;
    await page.setViewportSize({ width: 800, height: 1000 });
    await page
      .waitForFunction(
        ([id, w]) => document.getElementById(id)._fullLayout.width !== w,
        [first, wide],
        { timeout: 5000 },
      )
      .catch(() => {});
    for (const c of charts)
      r.charts[c].narrowWidth = (await chartState(page, c)).width;
    report.pages[name] = r;
  }
  report.bundles = [...bundles];
  report.errors = errors;
  fs.writeFileSync(
    path.join(out, 'report.json'),
    JSON.stringify(report, null, 2),
  );
  await browser.close();
  const count = Object.values(report.pages).reduce(
    (n, p) => n + Object.keys(p.charts).length,
    0,
  );
  console.log(
    `${Object.keys(report.pages).length} pages, ${count} charts, ` +
      `plotly ${report.plotly}, ${errors.length} console errors`,
  );
  for (const e of errors) console.log(`  ${e}`);
}

/** Compares two reports; returns whether they match (apart from the
 * plotly version and bundle URLs, which are expected to differ). */
function compare(dir1, dir2) {
  const load = (d) =>
    JSON.parse(fs.readFileSync(path.join(d, 'report.json'), 'utf8'));
  const [a, b] = [load(dir1), load(dir2)];
  console.log(`plotly: ${a.plotly} -> ${b.plotly}`);
  let same = true;
  for (const name of new Set([
    ...Object.keys(a.pages),
    ...Object.keys(b.pages),
  ])) {
    const pa = a.pages[name] || { charts: {} };
    const pb = b.pages[name] || { charts: {} };
    for (const c of new Set([
      ...Object.keys(pa.charts),
      ...Object.keys(pb.charts),
    ])) {
      const [x, y] = [
        JSON.stringify(pa.charts[c]),
        JSON.stringify(pb.charts[c]),
      ];
      if (x !== y) {
        same = false;
        console.log(`DIFF ${name} #${c}\n  ${dir1}: ${x}\n  ${dir2}: ${y}`);
      }
    }
  }
  for (const [d, r] of [
    [dir1, a],
    [dir2, b],
  ]) {
    if (r.errors.length) {
      same = false;
      console.log(`${d}: ${r.errors.length} console errors`);
    }
  }
  console.log(same ? 'The charts match.' : 'The charts differ.');
  return same;
}

if (process.argv[2] === '--compare') {
  if (process.argv.length !== 5) {
    console.error(`Usage: node ${process.argv[1]} --compare OUTDIR1 OUTDIR2`);
    process.exit(2);
  }
  process.exit(compare(process.argv[3], process.argv[4]) ? 0 : 1);
} else {
  const { outDir } = require('./lib.cjs');
  record(outDir()).catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
