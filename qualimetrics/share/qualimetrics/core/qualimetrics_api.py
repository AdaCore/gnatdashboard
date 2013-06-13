#########################################
#        Qualimetrics Plugin API        #
#########################################


## OutputParserMetaClass ######################################################
class OutputParserMetaClass(type):
    registered = dict()   # list of registered parsers

    def __new__(cls, name, bases, attrs):
        new_class = type.__new__(cls, name, bases, attrs)
        OutputParserMetaClass.registered[new_class.get_name()] = new_class
        return new_class

    def get_name(self):
        """Return the name of the parser, either from a "name" class
            attribute, or from the class name
        """
        return getattr(self, 'name', self.__name__).lower()

## OutputParser #####################################################xxxxxxxxx#
class OutputParser(object):
    __metaclass__ = OutputParserMetaClass

    def __init__(self,child):
        self.child = child

    def on_stdout(self,text):
        if self.child != None:
            self.child.on_stdout (text)

    def on_stderr(self,text):
        if self.child != None:
           self.child.on_stderr (text)

    def on_exit(self,status=0):
        if self.child != None:
            self.child.on_exit (status)

def create_parser(name, child=None):
    if OutputParserMetaClass.registered.has_key (name):
        return OutputParserMetaClass.registered[name](child)
    else:
        return None

def save_resource_message(file_path, rule_id, data, category=None):
    """Save message related to a file

       Parameters:
        - file_path: resource absolute path
        - rule_id: rule unique string identifier
        - data: description or value of the message
        - category: mesage category, None by default
    """
    print 'Saving message for resource: %s' % file_path
    print ' -- %s: %s' % (rule_id, data)

def save_entity_message (file_path, line, name, col_begin,
                         rule_id, data, category=None):
    """Save message related to a file

       Parameters:
        - file_path: resource absolute path where the entityt is located
        - line: line of the entity
        - name: entity name
        - col_begin: column begin of the entity
        - rule_id: rule unique string identifier
        - data: description or value of the message
        - category: mesage category, None by default
    """
    print 'Saving message for entity: %s' % name
    print ' -- %s: %s' % (rule_id, data)

