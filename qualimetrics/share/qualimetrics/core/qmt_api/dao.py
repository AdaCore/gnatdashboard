#########################################
#         Qualimetrics DOA API         #
#########################################

import sqlite3

## DAO ################################################################
##
class DAO(object):
    def __init__(self, connection):
        self.__connection = connection
        self.__cursor = self.__connection.cursor()

    def get_rule(self, rule_key):
        """Return rule from database if exists, None if not

           Parameters:
            - rule_key: rule unique string identifier
        """
        self.__cursor.execute('select * from rules r where r.identifier =:rule_key',
                            {'rule_key' : rule_key})
        return self.__cursor.fetchone()

    def save_tool(self, name):
        """Save tool

           Parameters:
            - name: name that designate the tool.
        """
        self.__cursor.execute('insert into tools (name) values (?)', (name,))
        self.__connection.commit()
        return

    def save_resource_message(self, tool_name, file_path, rule_key, data, category=None):
        """Save message related to a file

           Parameters:
            - tool_name: name of the tool associated to the rule
            - file_path: resource absolute path
            - rule_key: rule unique string identifier
            - data: description or value of the message
            - category: mesage category, None by default
        """
        return

    def save_entity_message (self, tool_name, file_path, line, name, col_begin,
                             rule_id, data, category=None):
        """Save message related to a file

           Parameters:
            - name of the tool associated to the rule
            - file_path: resource absolute path where the entityt is located
            - line: line of the entity
            - name: entity name
            - col_begin: column begin of the entity
            - rule_id: rule unique string identifier
            - data: description or value of the message
            - category: mesage category, None by default
        """
        return

