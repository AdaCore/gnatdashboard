"""Check DB content after spark2014 run."""

from unittest import TestCase
from support.mock import GNAThub, Project


class TestSpark2014Support(TestCase):
    def setUp(self):
        self.longMessage = True
        self.gnathub = GNAThub(Project.flow_analysis(), plugins=['spark2014'])
    
    def testFlowAnalysisDBContent(self):
        self.gnathub.run(script='check_flow_analysis_db.py')

    def testFlowAnalysisMultiObjDirDBContent(self):
        self.gnathub = GNAThub(Project.fa_multi_obj(), plugins=['spark2014'])
        self.gnathub.run(script='check_flow_analysis_multi_obj_db.py')
