#! /usr/bin/env python

"""
Simple Python script used to replace sqlite3 command-line tool.
This is used for testing purposes.
"""

import sys

from sqlalchemy import create_engine
from sqlalchemy.exc import SQLAlchemyError
from sqlalchemy.orm import sessionmaker


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print >> sys.stderr, 'Usage: %s DB_FILE SQL_REQUEST' % sys.argv[0]
        sys.exit(1)

    DB_FILE, SQL_REQUEST = sys.argv[1:3]

    __DB_ENGINE = create_engine('sqlite:///%s' % DB_FILE, echo=False)
    _DB_SESSION_FACTORY = sessionmaker()
    _DB_SESSION_FACTORY.configure(bind=__DB_ENGINE)

    SESSION = _DB_SESSION_FACTORY()

    try:
        RESULT = SESSION.execute(SQL_REQUEST)

        for row in RESULT:
            # Convert every thing in string (eg. in case we fetched integers)
            row = (str(cell) for cell in row)
            # Print a comma-separated list of the query's result
            print ','.join(row)

        SESSION.commit()

    except SQLAlchemyError as why:
        SESSION.rollback()
        print >> sys.stderr, 'Error in query: %s' % str(why)

    finally:
        SESSION.close()
