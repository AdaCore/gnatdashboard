import qual

# Define the metrics to use for this tool.
# We define a new metrics called "VCS" that inherits from Message

class VCS(qual.Message):

    def __init__(self, name, status, range=None):
        # The parent class requires a status and a text
        super(VCS, self).__init__("VCS", status, text=name, range=range)


# create Project, Dir and File objects.

project = qual.Project("my_project")  # this creates a project object
dir     = project.dir("src/")         # this creates a source directory object
file    = dir.file("hello.adb")       # this creates a source file object


# Add some metrics in my file
file.add(VCS("VC11", "OK", range=(12, 13)))
file.add(VCS("VC11", "OK", range=(13, 14)))
file.add(VCS("VC11", "OK", range=(22, 23)))
file.add(VCS("VC11", "UNKNOWN", range=(23, 24)))
file.add(VCS("VC11", "UNKNOWN", range=(31, 32)))
file.add(VCS("VC11", "UNKNOWN", range=(32, 33)))
file.add(VCS("VC11", "UNKNOWN", range=(41, 42)))
file.add(VCS("VC11", "UNKNOWN", range=(42, 43)))

# Dump the json object
j =  project.to_json()
print j

# Create a project by loading the json object and raise an error if this
# does not produce the same json
new_project = qual.Project.from_json(j)
j2 =  new_project.to_json()
if j != j2:
    print "difference between the first json and the second json"
    print j2
