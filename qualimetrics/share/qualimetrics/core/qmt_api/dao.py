#########################################
#         Qualimetrics DOA API         #
#########################################


## save_tool ######################################################
##
def save_tool(name):
    """Save tool

       Parameters:
        - name: name that designate the tool.
    """
    print "Saving: %s" % name
    return

## save_resource_message ######################################################
##
def save_resource_message(tool_name, file_path, rule_id, data, category=None):
    """Save message related to a file

       Parameters:
        - tool_name: name of the tool associated to the rule
        - file_path: resource absolute path
        - rule_id: rule unique string identifier
        - data: description or value of the message
        - category: mesage category, None by default
    """
    return

## save_entity_message ######################################################
##
def save_entity_message (tool_name, file_path, line, name, col_begin,
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
