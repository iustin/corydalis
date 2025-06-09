// build.js
import * as esbuild from 'esbuild';

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
      'js/fancybox.ts',
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
    // Start watch mode
    console.log('[watch] build started');
    await ctx.watch();
    console.log('Watch mode active. Press Ctrl+C to stop.');
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
