// esbuild.js
import * as esbuild from 'esbuild';
import {
  copyFileSync,
  mkdirSync,
  readdirSync,
  rmSync,
  watch as watchFiles,
} from 'node:fs';
import { writeStaticStamp } from './build/static-stamp.js';

const isDev = process.env.NODE_ENV === 'development';
const watch = process.argv.includes('--watch');

/**
 * @type {import('esbuild').Plugin}
 */
const esbuildProblemMatcherPlugin = {
  name: 'esbuild-problem-matcher',

  setup(build) {
    build.onStart(() => {
      //console.log('[watch] build started');
    });
    build.onEnd((result) => {
      result.errors.forEach(({ text, location }) => {
        console.error(`✘ [ERROR] ${text}`);
        console.error(
          `    ${location.file}:${location.line}:${location.column}:`,
        );
      });
      //console.log('[watch] build finished');
    });
  },
};

/**
 * The output directory, entirely generated: the bundles, the fonts, and
 * the files copied as they are (see `copiedFiles`).
 */
const outdir = 'static';

/** Files copied as they are into the output directory. */
const copiedFiles = ['manifest.json'];

/** Copies the files, printing the error if it fails (as esbuild does). */
function copyFiles() {
  try {
    for (const file of copiedFiles) {
      copyFileSync(file, `${outdir}/${file}`);
    }
  } catch (err) {
    console.error(`✘ [ERROR] Copying to ${outdir} failed: ${err.message}`);
    throw err;
  }
}

async function runBuild() {
  // The output is entirely generated, so empty it first (keeping the
  // directory itself): old files would otherwise stay around, e.g. the
  // fonts after an update (their names include a hash of their
  // contents), or the source maps of a development build in a
  // production one.
  mkdirSync(outdir, { recursive: true });
  for (const name of readdirSync(outdir)) {
    rmSync(`${outdir}/${name}`, { recursive: true, force: true });
  }

  // Create the build contexts: for the scripts, the theme script, and
  // the style sheets.
  const jsCtx = await esbuild.context({
    entryPoints: [
      'js/bundle-plot.ts',
      'js/bundle-basic.ts',
      'js/imagegrid.ts',
      'js/viewer.ts',
    ],
    bundle: true,
    minify: !isDev,
    format: 'esm',
    outdir: `${outdir}/js`,
    target: 'es2021',
    sourcemap: isDev,
    define: {
      __DEBUG__: isDev.toString(),
    },
    logLevel: 'info', // Show rebuild info in the console
    plugins: [esbuildProblemMatcherPlugin],
  });
  // The theme script runs as a classic (blocking) script in the pages'
  // head, see js/theme-boot.ts, hence its own context and format.
  const themeCtx = await esbuild.context({
    entryPoints: { theme: 'js/theme-boot.ts' },
    bundle: true,
    minify: !isDev,
    format: 'iife',
    outdir: `${outdir}/js`,
    target: 'es2021',
    sourcemap: isDev,
    logLevel: 'info',
    plugins: [esbuildProblemMatcherPlugin],
  });
  const cssCtx = await esbuild.context({
    entryPoints: ['css/bundle.css'],
    bundle: true,
    minify: !isDev,
    outdir,
    entryNames: 'css/[name]',
    // The fonts are copied next to the style sheet, which refers to them
    // by a relative URL; the hash in their names makes browsers fetch
    // them again when they change (their URLs have no etag parameter).
    loader: { '.woff2': 'file' },
    assetNames: 'fonts/[name]-[hash]',
    sourcemap: isDev,
    logLevel: 'info',
    // esbuild only warns about invalid CSS, which would ship as is.
    logOverride: { 'css-syntax-error': 'error' },
    plugins: [esbuildProblemMatcherPlugin],
  });
  const contexts = [jsCtx, themeCtx, cssCtx];
  // Let all the builds finish (and print their errors), then fail if any
  // of them failed.
  const rebuildAll = async () => {
    const results = await Promise.allSettled([
      ...contexts.map((ctx) => ctx.rebuild()),
      Promise.resolve().then(copyFiles),
    ]);
    const failed = results.find((r) => r.status === 'rejected');
    if (failed) {
      throw failed.reason;
    }
  };

  if (watch) {
    // esbuild's own watch mode polls every 100ms, even when idle. Instead,
    // build once, then rebuild only when the OS reports a change in js/ or
    // in the dependencies.
    const rebuild = async (reason) => {
      console.log(`[watch] build started${reason}`);
      await rebuildAll().catch(() => {}); // errors are already printed
      // Even after a failed build, as the files in static/ may have changed.
      try {
        writeStaticStamp();
      } catch (err) {
        console.error(
          '[watch] Writing the static files stamp failed, so Haskell builds' +
            ' keep the old URL hashes until it succeeds:',
          err,
        );
      }
      console.log('[watch] build finished');
    };
    await rebuild('');
    let timer;
    const scheduleRebuild = (file) => {
      // Editors often save in several steps (temp file, rename, ...), so
      // wait for things to settle before rebuilding.
      clearTimeout(timer);
      timer = setTimeout(() => rebuild(` (change: "${file}")`), 50);
    };
    for (const dir of ['js', 'css']) {
      watchFiles(dir, { recursive: true }, (_event, file) => {
        scheduleRebuild(`${dir}/${file}`);
      });
    }
    // `npm install` updates package-lock.json once node_modules is in
    // place. Watch the directory rather than the file itself, since a
    // file watch stops working if the file gets replaced via a rename.
    watchFiles('.', (_event, file) => {
      if (file === 'package-lock.json' || copiedFiles.includes(file)) {
        scheduleRebuild(file);
      }
    });
    console.log(
      `Watching js/, css/, package-lock.json and ${copiedFiles.join(', ')}` +
        ' for changes. Press Ctrl+C to stop.',
    );
  } else {
    // Just do a single build
    try {
      await rebuildAll();
    } finally {
      // Even after a failed build, as some bundles may have been written.
      writeStaticStamp();
    }
    console.log('Build complete!');
    await Promise.all(contexts.map((ctx) => ctx.dispose()));
  }
}

runBuild().catch((err) => {
  console.error('Build failed:', err);
  process.exit(1);
});
