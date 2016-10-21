JSON Entities
=============

Output format
-------------

JSON entities are encoded using UTF-8 and can be served with content type
`application/json`.

By default reporters output compact JSON, as opposed to pretty-printed JSON,
which uses extra whitespace to make the output more readable for humans. Use
GNAThub's `debug` switch to generate pretty-printed JSON reports.

Producing (and parsing) the non-pretty compact format is more efficient, so
tools can process the content of the reports effectively.

JSON reports can be further gzip compressed to save on local disk space and
network transfer time.

CoverageStatus
--------------

Coverage status apply to a single line of code.

* `NO_CODE`: this line does not contain or does not generate coverable code;`
* `COVERED`: all statements from this line were covered during tests;
* `NOT_COVERED`: no statements from this line were covered during tests;
* `PARTIALLY_COVERED`: some, but not all, statements from this line were covered
  during tests.

CoverageInfo
------------

The `CoverageInfo` entity contains detailled information about the coverage
results of a line of code.

+------------+----------------+------------------------------------------------+
| Field name | Field type     | Field description                              |
+============+================+================================================+
| status     | CoverageStatus | The coverage status for this line              |
+------------+----------------+------------------------------------------------+
| hits       | number         | The number of times this line was executed (if |
|            |                | available, `null` otherwise)                   |
+------------+----------------+------------------------------------------------+
