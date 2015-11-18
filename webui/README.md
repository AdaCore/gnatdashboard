GNATdashboard WebUI
===================

Setup environment
-----------------

The project dependencies are handled by [Node.js](https://nodejs.org).
First install Node.js on your system.

First, set up the environment:

    $ npm run start

This will (1) download/update all dependencies listed in `package.json` and
install them into the `node_modules` directory, (2) download/update the
Typings definition using the *tsd* program and store them in the `typings`
directory, (3) invoke `gulp build` to build the project in `DEBUG` mode.

To execute the testsuite, either use the `npm` script:

    $ npm run tests

*gulp* and *karma* are command-line tools that are installed as part of the `npm
install` step (executed by `npm start`), and are installed in the `node_modules`
directory. To use these tools, either specify the whole path (*e.g.*
`node node_modules/gulp/bin/gulp.js`) or use [npm](https://www.npmjs.com)
scripts defined in `package.json` (*via* `gulp run <script>`) or install the
[npm](https://www.npmjs.com) packages `gulp-cli` and `karma-cli` globally:

    $ npm install -g gulp-cli karma-cli

Depending on your system and installation, you may need a more privileged
access to run this command (*e.g.* by using `sudo`).

Build
-----

To build the web application:

    $ gulp build [--production]

To serve the web application, watch the sources and re-build on changes:

    $ gulp serve [--production]

Tests
-----

To execute the tests only once:

    $ karma start --single-run --no-auto-watch

or using the `gulp` integration:

    $ gulp tests

Alternatively, `karma` can run as a server, watching source files so that it
re-run builds and tests if a source file changes:

    $ karma start --no-single-run --auto-watch

or using the `gulp` integration:

    $ gulp tdd
