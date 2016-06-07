'use strict';

const $ = require('gulp-load-plugins')();
const gulp = $.param(require('gulp'), process.argv);

// PostCSS plugin
const mqpacker = require('css-mqpacker');       // Pack media queries
const cssnext = require('postcss-cssnext');     // Use latest CSS features
const csswring = require('csswring');           // Minimize CSS with sourcemaps
const reporter = require('postcss-reporter')    // PostCSS reporter
const sorting = require('postcss-sorting');     // Add support for sorted rules
const stylelint = require('stylelint');         // CSS linter

// Gulp utilities
const del = require('del');                     // Remove build artifacts
const runSequence = require('run-sequence');    // Run tasks sequentially
const eventStream = require('event-stream');    // Merge two Node.js streams

// Development utilities
const browserSync = require('browser-sync');    // Auto-reload

/*
 * Configuration
 */

// Tasks name
const tasks = {
  'default': 'default',
  build: 'build',
  bundle: 'bundle',
  browserSync: 'browser-sync',
  check: 'check',
  checkCSS: 'check-css',
  checkTS: 'check-ts',
  clean: 'clean',
  compress: 'compress',
  dist: 'dist',
  genJS: 'gen-js',
  genCSS: 'gen-css',
  html: 'html',
  serve: 'serve',
  watch: 'watch'
};

// Path to the configuration files
const config = {
  karma: __dirname + '/karma.conf.js',
  size: { showFiles: true },
  typescript: 'tsconfig.json'
};

// Path to the input files
const sources = {
  html: 'src/app/*.html',
  css: 'src/app/css/**/*.css',
  ts: 'src/app/ts/**/*.ts',
  templates: 'src/app/ts/**/*.html',
  typings: 'src/app/typings/**/*.ts'
};

// Path to the output files / directories
const build = {
  app: 'build/app',
  dist: 'dist',
  out: 'build',
  static: 'build/app/static'
};

/*
 * Tasks
 */

// Compile TypeScript files into JavaScript
gulp.task(tasks.genJS, function() {
  const project = $.typescript.createProject(config.typescript)
  return gulp.src([sources.ts, sources.typings])
    .pipe($.sourcemaps.init())
    .pipe($.typescript(project))
    .pipe($.size(config.size))
    .pipe($.sourcemaps.write('maps'))
    .pipe(gulp.dest(build.app))
    .pipe(browserSync.stream());
});

// Run the linter on all TypeScript files (see tslint.json)
gulp.task(tasks.checkTS, function(production) {
  const options = { summarizeFailureOutput: true };
  if (!production) {
    options['emitError'] = false;
  }
  return gulp.src(sources.ts)
    .pipe($.tslint())
    .pipe($.tslint.report('prose', options));
});

// Run the linter on all CSS files (see stylelint-config-standard)
gulp.task(tasks.checkCSS, function() {
  return gulp.src(sources.css)
    .pipe($.postcss([stylelint, reporter({ clearMessages: true })]));
});

// Process CSS files into optimized CSS
gulp.task(tasks.genCSS, function(production) {
  const processors = [cssnext];

  if (production) {
    processors.push(mqpacker);
    processors.push(csswring);
  } else {
    processors.push(sorting);
  }

  return gulp.src(sources.css)
    .pipe($.sourcemaps.init())
    .pipe($.postcss(processors))
    .pipe($.size(config.size))
    .pipe($.sourcemaps.write('maps'))
    .pipe(gulp.dest(build.app));
});

// Gzip JS and CSS assets
gulp.task(tasks.compress, function() {
  return gulp.src([build.static + '/*.js', build.static + '/*.css'])
    .pipe($.gzip())
    .pipe(gulp.dest(build.static))
    .pipe($.size(config.size));
});

// Parse HTML and concat JavaScript and CSS files
gulp.task(tasks.html, function(production) {
  return eventStream.merge(
    gulp.src(sources.templates, { base: 'src/app/ts' })
      .pipe(gulp.dest(build.app)),
    () => {
      if (!production) {
        return gulp.src(sources.html);

      } else {
        const assets = $.useref.assets();
        return gulp.src(sources.html)
          .pipe(assets)
            .pipe($.if('*.js', $.uglify()))   // Minimize JavaScript files only
            .pipe($.rev())
          .pipe(assets.restore())
          .pipe($.useref())
          .pipe($.revReplace());
      }
    }().pipe(gulp.dest(build.app))
  ).pipe(browserSync.stream()).pipe($.size(config.size));
});

// Bundle the application
gulp.task(tasks.bundle, function() {
  return gulp.src([build.app + '/index.html', build.static + '/**/*'])
    .pipe($.size(config.size))
    .pipe($.copy(build.dist, { prefix: 2 }));
});

// Create the application package
gulp.task(tasks.dist, function(production) {
  if (!production) {
    throw new $.util.PluginError({
      plugin: 'sanity-check',
      message: 'Building in DEBUG mode; please use --production explicitely.'
    });
  }
  runSequence(
    tasks.clean, tasks.check, [tasks.genJS, tasks.genCSS], tasks.html,
    tasks.compress, tasks.bundle);
});

// Remove the build artifacts
gulp.task(tasks.clean, function(callback) {
  return del([build.out, build.dist], callback);
});

// Run all linters
gulp.task(tasks.check, [tasks.checkTS, tasks.checkCSS]);

// Build for Development or Production mode (use --production on gulp cmdline)
gulp.task(tasks.build, function() {
  runSequence(tasks.check, [tasks.genJS, tasks.genCSS], tasks.html);
});

// Rebuild watcher
gulp.task(tasks.watch, function() {
  gulp.watch([sources.html, sources.ts, sources.css], [tasks.build]);
});

// Serve the application with browser-sync
gulp.task(tasks.browserSync, function(production) {
  browserSync([(production ? build.dist : build.app) + "/**/*.css"], {
    injectChanges: true,
    server: {
      baseDir: production ? build.dist : build.app,
      routes: {
        '/angular2': 'node_modules/angular2',
        '/app': 'build/app',
        '/build': 'build',
        '/node_modules': 'node_modules',
        '/rxjs': 'node_modules/rxjs',
        '/highlight.js': 'node_modules/highlight.js/lib/highlight.js',
      }
    }
  }, function(err, bs) {
    bs.addMiddleware('', function(req, res, next) {
      if (req.url === '/api/report/gnatcoverage') {
        return function(req, res) {
          res.setHeader('Content-Type', 'application/json');
          return res.end(
            require('fs').readFileSync('mocks/gnatcoverage-report.json')
          );
        }(req, res);
      } else if (req.url === '/api/report/source/zip_stream.adb') {
        return function(req, res) {
          res.setHeader('Content-Type', 'application/json');
          return res.end(
            require('fs').readFileSync('mocks/zip_stream.adb.json')
          );
        }(req, res);
      } else if (req.url === '/api/report/gnathub') {
        return function(req, res) {
          res.setHeader('Content-Type', 'application/json');
          return res.end(
            require('fs').readFileSync('mocks/gnathub-report.json')
          );
        }(req, res);
      }
      next();
    }, { override: true });
  });
});

// Watch source files and run local server with auto-reload capabilities
gulp.task(tasks.serve, [tasks.browserSync], function(dist) {
  gulp.watch([sources.html, sources.templates], [tasks.html]);

  if (dist) {
    gulp.watch([sources.ts, sources.typing, sources.css], [tasks.dist]);
  } else {
    gulp.watch([sources.ts, sources.typings], [tasks.genJS]);
    gulp.watch([sources.css], [tasks.genCSS]);
  }
});

// Default task: build
gulp.task(tasks.default, [tasks.build]);
