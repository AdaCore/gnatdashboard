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
            output.browsers.forEach(function(browser) {
                browser.results.forEach(function(result) {
                    myResults += '[' + result.suite[0] + '] ' +
                        result.description + ':';
                    myResults += (result.success ? 'OK:' : 'FAIL:');
                    result.log.forEach(function(myLog) {
                        if (myLog.split(' ')[0] === 'Expected') {
                            myResults += myLog;
                        }
                    });
                    myResults += '\n';
                });
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

    function logMessageFormater(error) {
        return formatError(error);
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
        var browserResults = [];

        for (var browserId in self.browsers) {
            var browser = self.browsers[browserId];
            browser.errors = browser.errors.map(logMessageFormater);
            browserResults.push(browser);
        }

        var output = {
            summary: summary,
            browsers: browserResults
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
