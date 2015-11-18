'use strict';

module.exports = function(config) {
  config.set({

    // Base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: './',

    // List of files / patterns to load in the browser
    files: [
      'node_modules/zone.js/dist/zone-microtask.js',
      'node_modules/zone.js/dist/long-stack-trace-zone.js',
      'node_modules/zone.js/dist/jasmine-patch.js',
      'node_modules/es6-module-loader/dist/es6-module-loader.js',
      'node_modules/traceur/bin/traceur-runtime.js', // Required by PhantomJS2, otherwise it shouts ReferenceError: Can't find variable: require
      'node_modules/traceur/bin/traceur.js',
      'node_modules/systemjs/dist/system.src.js',
      'node_modules/reflect-metadata/Reflect.js',

      { pattern: 'node_modules/angular2/**/*.js', included: false, watched: false },
      { pattern: 'node_modules/@reactivex/rxjs/dist/**/*.js', included: false, watched: false },
      { pattern: 'node_modules/systemjs/dist/system-polyfills.js', included: false, watched: false }, // PhantomJS2 (and possibly others) might require it

      // Comment out the following line if duplicate symbols errors are
      // generated, which happens when using some AngularJS features (because
      // then the definitions get implicitely included).
      'typings/tsd.d.ts',

      'src/app/ts/**/*.spec.ts'
    ],

    // List of files to exclude
    exclude: [
      'node_modules/angular2/**/*_spec.js',
    ],

    // See: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['mocha'],
    // See: https://npmjs.org/browse/keyword/karma-launcher
    browsers: ['PhantomJS2'],
    // See: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['browserify', 'jasmine'],
    plugins: [
      'karma-browserify',
      'karma-jasmine',
      'karma-mocha-reporter',
      'karma-chrome-launcher',
      'karma-phantomjs2-launcher',
      'karma-typescript-preprocessor2'
    ],
    // See: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
      '**/*.ts': ['browserify'],
    },
    browserify: {
      debug: true,
      plugin: ['tsify']
    },

    port: 9876,
    colors: true,
    logLevel: config.LOG_INFO,  // LOG_DISABLE, LOG_ERROR, LOG_WARN, LOG_DEBUG
    autoWatch: false,
    singleRun: true,
    concurrency: Infinity  // How many browser should be started simultanous
  });
};
