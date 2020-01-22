var path = require('path');
var fs = require('fs');

function writeOutput(config, output, helper, logger) {

    var log = logger.create('karma-adacore-reporter');
    if (config.dir) {

        let output_json = config.dir + '/output.json';
        let results = config.dir + '/results';

        //write the json file
        helper.mkdirIfNotExists(path.dirname(output_json), function() {
            fs.writeFile(output_json, JSON.stringify(output, null, 4),
                         function(err) {
                if (err) {
                    log.warn('Cannot write test results to JSON file\n\t' +
                             err.message);
                } else {
                    log.debug('Test results were written to JSON file ' +
                              config.dir);
                }
            });
        });

        //write results
        helper.mkdirIfNotExists(path.dirname(results), function() {
            let myResults = '';

            Object.keys(output.tests).forEach(function(key, idx) {
                myResults += output.tests[key].key + ':';
                myResults += output.tests[key].status + ':';
                myResults += output.tests[key].comment + '\n';
            });

            fs.writeFile(results, myResults, function(err) {
                if (err) {
                    log.warn('Cannot write test results to JSON file\n\t' +
                             err.message);
                } else {
                    log.debug('Test results were written to JSON file ' +
                              config.dir);
                }
            });
        });
    } else {
        process.stdout.write(JSON.stringify(output));
    }
}

var AdacoreReporter = function(baseReporterDecorator,
                                formatError, config, helper, logger) {

    var self = this;

    baseReporterDecorator(self);

    function resultsFormatter(gaia_results, resultsArray) {
        resultsArray.forEach(function(result) {
            var myDiff = '';
            var myStatus = result.success ? 'OK' : 'FAIL';
            if (result.description.match('XFAIL')) {
                myStatus = 'XFAIL';
            }
            var myComment = (myStatus === 'XFAIL' ?
                             'This test is bad on purpose' : '');

            if (result.log) {
                result.log.forEach(function(message) {
                    myDiff += message + '\n';
                });
            }

            var gaia_object = {
                status: myStatus,
                diff: myDiff,
                comment: myComment,
                key: result.description
            };
            gaia_results[result.description] = gaia_object;
        });

        return gaia_results;
    };

    function getBrowser(browser) {
        var b = self.browsers[browser.id];

        if (b) {
            return b;
        }

        var newRecord = {
            browser: browser,
            errors: [],
            results: []
        };

        self.browsers[browser.id] = newRecord;

        return newRecord;
    }

    self.clear = function() {
        self.browsers = {};
    };

    self.onBrowserError = function(browser, error) {
        getBrowser(browser).errors.push(error);
    };

    self.onSpecComplete = function(browser, result) {
        // convert newlines into array and flatten
        result.log = [].concat.apply([], result.log.map(function(message) {
            return message.split('\n');
        }));
        getBrowser(browser).results.push(result);
    };

    self.onRunComplete = function(browsers, summary) {
        var comment = '';
        var gaia_results = {};

        for (var browserId in self.browsers) {
            var browser = self.browsers[browserId];
            gaia_results = resultsFormatter(gaia_results, browser.results);
            comment += browser.browser.fullName;
        }

        var output = {
            comments: comment,
            tests: gaia_results
        };

        writeOutput(config, output, helper, logger);
        self.clear();
    };
    self.clear();
};

AdacoreReporter.$inject = ['baseReporterDecorator', 'formatError',
                           'config.adacoreReporter', 'helper', 'logger'];

module.exports = {
    'reporter:adacore-reporter': ['type', AdacoreReporter]
};
