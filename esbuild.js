// build.js
import * as esbuild from 'esbuild';
import { watch as watchFiles } from 'node:fs';

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

async function runBuild() {
  // Create a build context
  const ctx = await esbuild.context({
    entryPoints: [
      'js/bundle-plot.ts',
      'js/bundle-basic.ts',
      'js/bundle-table.ts',
      'js/imagegrid.ts',
      'js/tablesorter-config.ts',
      'js/tablesorter-uitheme-simple.ts',
      'js/viewer.ts',
    ],
    bundle: true,
    minify: !isDev,
    format: 'esm',
    outdir: 'static/corydalis/js',
    target: 'es2021',
    sourcemap: isDev,
    define: {
      __DEBUG__: isDev.toString(),
    },
    logLevel: 'info', // Show rebuild info in the console
    plugins: [esbuildProblemMatcherPlugin],
  });

  if (watch) {
    // esbuild's own watch mode polls every 100ms, even when idle. Instead,
    // build once, then rebuild only when the OS reports a change in js/ or
    // in the dependencies.
    const rebuild = async (reason) => {
      console.log(`[watch] build started${reason}`);
      await ctx.rebuild().catch(() => {}); // errors are already printed
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
    watchFiles('js', { recursive: true }, (_event, file) => {
      scheduleRebuild(`js/${file}`);
    });
    // `npm install` updates package-lock.json once node_modules is in
    // place. Watch the directory rather than the file itself, since a
    // file watch stops working if the file gets replaced via a rename.
    watchFiles('.', (_event, file) => {
      if (file === 'package-lock.json') {
        scheduleRebuild(file);
      }
    });
    console.log(
      'Watching js/ and package-lock.json for changes. Press Ctrl+C to stop.',
    );
  } else {
    // Just do a single build
    const result = await ctx.rebuild();
    console.log(
      `Build complete! Processed ${result?.outputFiles?.length || 'unknown number of'} files.`,
    );
    await ctx.dispose();
  }
}

runBuild().catch((err) => {
  console.error('Build failed:', err);
  process.exit(1);
});
