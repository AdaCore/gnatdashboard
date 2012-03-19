import json

"""
A source element can be:

   * a project
      * a directory
         * a file
            * a subprogram
            * a line
            * a sloc (file + line + column)
            * a range (source location start + source location end)

The following data can be attached source elements:

       * message:
           * val : a numerical value
           * status (optional): the name of the category of the message
           * text (optional): a string

"""

class Source_Element(object):

    def __init__(self):
        self.data = {}
        # Keys:   name of tool
        # Values: list of data

    def add(self, data):
        if data.tool in self.data:
            self.data[data.tool].append(data)
        else:
            self.data[data.tool] = [data]

    def data_to_json(self):
        """ Return a json-ready python object representing Source_Element """
        r = {}
        for tool in self.data:
            r[tool] = [d.to_json() for d in self.data[tool]]
        return r

    def data_from_json(self, json):
        """ create self.data from a json object
        """
        for tool in json:
            for d in json[tool]:
                data = Message.from_json(tool, d)
                self.add (data)

    @classmethod
    def from_json(cls, json):
        """ Construct a Source_Element from the given element """
        raise "attempting to use from_json on a class that didn't overwrite it"

class Project(Source_Element):

    def __init__ (self, name):
        """ name: the project name, for instance "My_Project".
        """
        self.name = name
        self.dirs = {}
        # The source directories. Keys: directory names. Values: Dir
        super (Project, self).__init__()


    def dir(self, name):
        """ Create a Dir object and add it to this project.
        """
        dir = Directory(name)
        self.dirs[name] = dir
        return dir


    def to_json(self):
        """ Return a json-ready python object representing Source_Element """
        res =  {'name' : self.name,
                'dirs' : [self.dirs[d].to_json() for d in self.dirs],
                'data' : self.data_to_json()}
        return res

    @classmethod
    def from_json(cls, json):
        """ Construct an instance of this class from the given json data """
        p = cls(json['name'])
        p.dirs = {}
        for dir in [Directory.from_json(j) for j in json['dirs']]:
            p.dirs[dir.name] = dir
        p.data_from_json(json['data'])
        return p


class Directory(Source_Element):

    def __init__ (self, name):
        """ name: the name of the directory, as it appears in the project.

            For example, if the project defines
                for Source_Dirs use ("src/foo/");

            then a directory should be created with name = "src/foo"
        """

        self.name = name
        self.files = {}
        # The source files. Keys: file base names. Values: Dir

        super (Directory, self).__init__()

    def file(self, basename):
        """ Create a File object and add it to this file
        """
        file = File(basename)
        self.files[file.basename] = file
        return file

    def to_json(self):
        """ Return a json-ready python object representing Source_Element """
        return {'name'  : self.name,
                'files' : [self.files[k].to_json() for k in self.files],
                'data'  : self.data_to_json()}

    @classmethod
    def from_json(cls, json):
        """ Construct an instance of this class from the given json data """
        p = cls(json['name'])
        p.files = {}
        for j in json['files']:
            file = File.from_json(j)
            p.files[file.basename] = file
        p.data_from_json(json['data'])
        return p


class File(Source_Element):

    def __init__ (self, basename):
        """ dir: a Dir
            basename: the base file name
        """
        self.basename = basename

        self.subprograms = {}
        # Keys: (subprogram name, sloc)
        # Values: Subprogram

        self.lines = {}
        # Keys: line numbers. Values: Lines

        self.ranges = {}
        # Keys: start/end sloc

        self.sloc = {}
        # Keys: (line, column)
        # Values: Sloc

        super (File, self).__init__()

    def to_json(self):
        """ Return a json-ready python object representing Source_Element """
        return {'basename' : self.basename,
                'data'  : self.data_to_json()}

    @classmethod
    def from_json(cls, json):
        """ Construct an instance of this class from the given json data """
        p = cls(json['basename'])
        p.data_from_json(json['data'])
        return p


class Message(object):

    def __init__(self, tool, status, text=None, val=None, extra=None,
                 subprogram=None, line=None, sloc=None, range=None):
        """
            tool: a string that identifies the emitter of the message,
                  for instance "gcov" or "codepeer" or "gnatmetric"

            status: a string, identifying the category to attach to the message.
                 Each tool defines their status
                 for instance for "codepeer" we would have categories
                  "low", "medium", "high". For "gcov" we could have categories
                  "c" and "n" for covered and not-covered lines.

                 This string is used to colorize lines or order messages, for
                 instance.

            text: a string associated with the message

            val: a numeric value.

            extra: any json-ready data (combination of dictionary, lists,
              strings, and numeric values. This is information that is stored
              in the JSON for any purpose but not displayed in the reports.

            The location of the message can be refined by specifying one or
            more of the following fields:

               subprogram: a string, the name of the subprogram to attach
                 this message to.

               line: a line number

               sloc: a tuple (line, column)

               range: can be either a couple of lines:
                     (line1, line2)
                   or a couple of slocs:
                     (line1, col1, line2, col2)

        """

        self.tool = tool
        self.status = status
        self.text = text
        self.val = val
        self.extra = extra
        self.subprogram = subprogram
        self.line = line
        self.sloc = sloc
        self.range = range

    def to_json(self):
        """ Return a json-ready python object representing Source_Element """
        d = {'status' : self.status }
        if self.text:
            d['text'] = self.text
        if self.val:
            d['val'] = self.val
        if self.extra:
            d['extra'] = self.extra
        if self.subprogram:
            d['subprogram'] = self.subprogram
        if self.line:
            d['line'] = self.line
        if self.sloc:
            d['sloc'] = self.sloc
        if self.range:
            d['range'] = self.range
        return d

    @classmethod
    def from_json(cls, tool, json):
        """ Construct an instance of this class from the given json data """
        p = cls(tool, json['status'])
        if 'text' in json:
            p.text = json['text']
        if 'val' in json:
            p.val = json['val']
        if 'extra' in json:
            p.extra = json['extra']
        if 'subprogram' in json:
            p.subprogram = json['subprogram']
        if 'line' in json:
            p.line = json['line']
        if 'sloc' in json:
            p.sloc = json['sloc']
        if 'range' in json:
            p.range = json['range']
        return p
