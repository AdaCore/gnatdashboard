"""Check that object folder is changed and all files have been created."""

import os.path

from unittest import TestCase
from support.mock import GNAThub, Project

class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

    def testSonarWorkDirSwitch(self):
        PROJECT = Project.simple()
        PLUGINS = ['sonar-config']

        # Define 'obj' folder name and path
        OBJ_FOLDER = 'obj'
        OBJ_PATH = os.path.join(PROJECT.install_dir, OBJ_FOLDER)
        SONAR_DEFAULT = 'sonar'
        SONAR_DEFAULT_PATH = os.path.join(PROJECT.install_dir, OBJ_FOLDER, "gnathub", SONAR_DEFAULT)
        
        # Define 'sq_copy' as target folder name and path of --sonar-work-dir switch
        SQ_WORK_FOLDER = 'sq_copy'
        SQ_WORK_PATH = os.path.join(PROJECT.install_dir, SQ_WORK_FOLDER)

        # 1st step: Run GNAThub without '--sonar-work-dir' switch with only sonar-config plugin
        assert not os.path.exists(OBJ_PATH), 'Object folder should not exist'
        gnathub = GNAThub(PROJECT, plugins=PLUGINS)

        assert os.path.exists(OBJ_PATH), 'Object folder should exist'
        assert os.path.exists(SONAR_DEFAULT_PATH), 'Sonar default folder should exist'
        assert not os.path.exists(SQ_WORK_PATH), 'Custom sonar work folder should not exist'

        # Check that metrix and gnatcheck results are generated in /obj/gnathub/sonar
        SONAR_PROJECT_PROPERTIES_FILE_FOUND = False
        SOURCES_MAPPING_FILE_FOUND = False

        SOURCES_CACHE_DIR_FOUND = False

        for entry in os.listdir(SONAR_DEFAULT_PATH):
            if entry.endswith('.properties') and entry.startswith('sonar-'):
                SONAR_PROJECT_PROPERTIES_FILE_FOUND = True
            elif entry.endswith('.properties') and entry.startswith('sources-'):
                SOURCES_MAPPING_FILE_FOUND = True
            elif os.path.isdir(os.path.join(SONAR_DEFAULT_PATH, entry)):
                if entry == 'sources.cache':
                    SOURCES_CACHE_DIR_FOUND = True

        assert SONAR_PROJECT_PROPERTIES_FILE_FOUND, 'sonar-project.properties should be in the default sonar folder'
        assert SOURCES_MAPPING_FILE_FOUND, 'sources-mapping.properties should be in the default sonar folder'
        assert SOURCES_CACHE_DIR_FOUND, 'sources.cache directory should be in the default sonar folder'

        # 2nd step: Run GNAThub with option '--sonar-work-dir=/sq_copy' only with sonar-config plugin
        gnathub = GNAThub(PROJECT, plugins=PLUGINS, sonar_work_dir=SQ_WORK_PATH)
        assert os.path.exists(SQ_WORK_PATH), 'Custom sonar sq_copy folder should have been created'

        # Check that configuration files and sources.cache folder are present in '/sq_copy/' in project install dir
        CUSTOM_SONAR_PROJECT_PROPERTIES_FILE_FOUND = False
        CUSTOM_SOURCES_MAPPING_FILE_FOUND = False
        CUSTOM_SOURCES_CACHE_DIR_FOUND = False

        for entry in os.listdir(SQ_WORK_PATH):
            if entry.endswith('.properties') and entry.startswith('sonar-'):
                CUSTOM_SONAR_PROJECT_PROPERTIES_FILE_FOUND = True
            elif entry.endswith('.properties') and entry.startswith('sources-'):
                CUSTOM_SOURCES_MAPPING_FILE_FOUND = True
            elif os.path.isdir(os.path.join(SONAR_DEFAULT_PATH, entry)):
                if entry == 'sources.cache':
                    CUSTOM_SOURCES_CACHE_DIR_FOUND = True

        assert CUSTOM_SONAR_PROJECT_PROPERTIES_FILE_FOUND, 'sonar-project.properties should be in the custom folder'
        assert CUSTOM_SOURCES_MAPPING_FILE_FOUND, 'sources-mapping.properties should be in the custom folder'
        assert CUSTOM_SOURCES_CACHE_DIR_FOUND, 'sources.cache directory should be in the custom folder'
