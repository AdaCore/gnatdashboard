##############################################################################
##                                                                          ##
##                               G N A T h u b                              ##
##                                                                          ##
##                        Copyright (C) 2013, AdaCore                       ##
##                                                                          ##
## The QM is free software; you can redistribute it  and/or modify it       ##
## under terms of the GNU General Public License as published by the Free   ##
## Software Foundation; either version 3, or (at your option) any later     ##
## version.  The QM is distributed in the hope that it will be useful,      ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHAN-  ##
## TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public ##
## License  for more details. You  should  have  received a copy of the GNU ##
## General Public License  distributed with the QM; see file COPYING3. If   ##
## not, write  to  the Free  Software  Foundation,  59 Temple Place - Suite ##
## 330, Boston, MA 02111-1307, USA.                                         ##
##                                                                          ##
##############################################################################

"""???"""

import GPS

from GNAThub.db import Category, Line, Rule, Resource, Tool


def get_or_create_rule(session, tool, kind, identifier, name=None):
    """Get rule for the given identifier if exists in the database, if it does
       not, create a new Rule objet in the given session. The rule is not
       persisted to the database. A session.commit() is required for that, this
       call is left for the one that manages the session.

       Parameters:
        - session: sqlachemy session
        - identifier: identifier of the rule
        - name: name of the rule
        - tool: tool which the rule is attached to
    """

    rule = session.query(Rule).filter_by(identifier=identifier,
                                         kind=kind).first()

    if not rule:
        if not name:
            name = identifier
        rule = Rule(identifier, name, tool, kind)

    return rule


def get_or_create_category(session, label):
    """Returns the category pointed to by label.

    Creates the category if it does not exists yet.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param label: the category label
        :type label: a string

    RETURNS
        the category
        :rtype: a GNAThub.db.Category object
    """

    category = session.query(Category).filter_by(label=label).first()

    if not category:
        category = Category(label)

    return category


def get_file(session, filename):
    """Returns the Resource object for the given file path.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param filename: the file name
        :type filename: a string

    RETURNS
        :rtype: a GNAThub.db.Resource object
    """

    dao = session.query(Resource)\
        .filter_by(kind=2)\
        .filter_by(name=GPS.File(filename).name())\
        .first()

    return dao


def get_file_by_id(session, resource_id):
    """Returns the Resource object for the given file path.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param filename: the file name
        :type filename: a string

    RETURNS
        :rtype: a GNAThub.db.Resource object
    """

    resource = session.query(Resource)\
        .filter_by(id=resource_id)\
        .first()

    return resource


def get_resource(session, name):
    """Returns the Resource object for the given file path.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param name: the resource name
        :type name: a string

    RETURNS
        :rtype: a GNAThub.db.Resource object
    """

    resource = session.query(Resource)\
        .filter(Resource.name.like('%' + name + '_'))\
        .first()

    return resource


def get_or_create_line(session, filename, line_num):
    """Returns the Line object for the given file and line number, or None if
    the file is not referenced in the database.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param filename: the resource name
        :type filename: a string
        :param line_num: the line number
        :type line_num: number

    RETURNS
        :rtype: a GNAThub.db.Line object
    """

    line = None

    if filename:
        line = session.query(Line)\
            .filter_by(line=line_num)\
            .join(Line.resource)\
            .filter(Resource.name.like('%' + filename))\
            .first()

        if not line:
            resource = get_file(session, filename)

            if resource:
                line = Line(line_num, resource)

    return line


# pylint: disable=C0103
# Disable Invalid identifier (too long)
def get_or_create_line_from_resource_id(session, resource_id, line_num):
    """Returns the line object for the given file and line number, or None if
    the file is not referenced in the database.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param resource_id: the resource identifier
        :type resource_id: a string
        :param line_num: the line number
        :type line_num: number

    RETURNS
        :rtype: a GNAThub.db.Line object
    """

    line = None

    if resource_id:
        line = session.query(Line) \
            .filter_by(line=line_num) \
            .join(Line.resource) \
            .filter(Resource.id == resource_id) \
            .first()

        if not line:
            resource = get_file_by_id(session, resource_id)

            if resource:
                line = Line(line_num, resource)

    return line


def get_sub_project(session, name):
    """Returns the subproject whose name is pointed to by name.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param name: the sub-project name
        :type name: a string

    RETURNS
        :rtype: a string
    """

    return session.query(Resource).filter_by(name=name, kind=0).first()


def save_tool(session, name):
    """Stores the tool in the database and returns it.

    PARAMETERS
        :param session: the database session
        :type session: sqlalchemy.orm.Session
        :param name: the tool name
        :type name: a string

    RETURNS
        :rtype: a GNAThub.db.Tool object
    """

    tool = Tool(name)
    session.add(tool)

    return tool
