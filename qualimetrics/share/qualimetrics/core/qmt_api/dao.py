from db import Rule, Resource, Tool

## get_or_create_rule #########################################################
##
def get_or_create_rule(session, identifier, name, tool):
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
    rule = session.query(Rule).filter_by(identifier=identifier).first()
    if not rule:
        rule = Rule(identifier, name, tool)
    return rule

def get_file(session, file_path):
    """Return the File object for the given file path """
    file = session.query(Resource).filter_by(name=file_path).first()
    return file

def save_tool(session, name):
   tool = Tool(name)
   session.add(tool)
   return tool

