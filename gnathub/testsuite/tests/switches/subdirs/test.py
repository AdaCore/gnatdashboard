"""Check that object folder is changed and all files have been created."""

import os.path

from unittest import TestCase
from support.mock import GNAThub, Project

class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

    def testSubdirsSwitch(self):
        PROJECT = Project.simple()
        PLUGINS = ['gnatmetric', 'gnatcheck', 'codepeer']

        # Define 'obj' folder name and path
        OBJ_FOLDER = 'obj'
        OBJ_PATH = os.path.join(PROJECT.install_dir, OBJ_FOLDER)

        # Define 'test_metrix' as target folder name and path of --subdirs switch
        SUBDIRS_FOLDER = 'test_metrix'
        NEW_OBJ_PATH = os.path.join(PROJECT.install_dir, OBJ_FOLDER, SUBDIRS_FOLDER)

        # 1st step of test : Run GNAThub without '--subdirs' switch only for gnatmetric plugin
        assert not os.path.exists(OBJ_PATH), 'Object folder should not exist'
        gnathub = GNAThub(PROJECT, plugins=PLUGINS)

        assert os.path.exists(OBJ_PATH), 'Object folder should exist'
        assert not os.path.exists(NEW_OBJ_PATH), 'New object folder should not have been created yet'

        # Check that metrix and gnatcheck results are generated in /obj
        METRIX_FILES_COUNT = 0
        METRIX_FILES_FOUND = False

        GNATCHECK_FILES_COUNT = 0
        GNATCHECK_FILES_FOUND = False

        GNATHUB_DIR_FOUND = False
        GNATHUB_DIR_LEN = 0

        CODEPEER_DIR_FOUND = False
        CODEPEER_DIR_LEN = 0

        for entry in os.listdir(OBJ_PATH):
            if entry.endswith('.metrix') or entry.startswith('metrix'):
                METRIX_FILES_COUNT += 1
            elif entry.startswith('gnatcheck') and entry.endswith('.out'):
                GNATCHECK_FILES_COUNT += 1
            elif os.path.isdir(os.path.join(OBJ_PATH, entry)):
                crt_path = os.path.join(OBJ_PATH, entry)
                if entry == 'gnathub':
                    GNATHUB_DIR_FOUND = True
                    GNATHUB_DIR_LEN = len(os.listdir(crt_path))
                elif entry == 'codepeer':
                    CODEPEER_DIR_FOUND = True
                    CODEPEER_DIR_LEN = len(os.listdir(crt_path))

        METRIX_FILE_FOUND = METRIX_FILES_COUNT != 0
        GNATCHECK_FILES_FOUND = GNATCHECK_FILES_COUNT == 2 # expected value is 2

        assert METRIX_FILE_FOUND, 'Metrix should be generated in /obj folder'
        assert GNATCHECK_FILES_FOUND, 'Gnatcheck out files should be generated in /obj folder'
        assert CODEPEER_DIR_FOUND, 'Codepeer directory should be generated in /obj folder'

        # Run GNAThub with option '--subdirs=test_metrix' only for gnatmetric plugin
        gnathub = GNAThub(PROJECT, plugins=PLUGINS, subdirs=SUBDIRS_FOLDER)
        assert os.path.exists(NEW_OBJ_PATH), 'New object folder should have been created'

        # Check that metrix and gnatcheck results are present in '/obj/test_metrix' folder
        NEW_METRIX_FILES_COUNT = 0
        NEW_GNATCHECK_FILES_COUNT = 0

        NEW_GNATHUB_DIR_FOUND = False
        NEW_GNATHUB_DIR_LEN = 0

        NEW_CODEPEER_DIR_FOUND = False
        NEW_CODEPEER_DIR_LEN = 0

        for entry in os.listdir(NEW_OBJ_PATH):
            filename, ext = os.path.splitext(entry)
            if entry.endswith('.metrix') or entry.startswith('metrix'):
                NEW_METRIX_FILES_COUNT += 1
            elif entry.startswith('gnatcheck') and entry.endswith('.out'):
                NEW_GNATCHECK_FILES_COUNT += 1
            elif os.path.isdir(os.path.join(NEW_OBJ_PATH, entry)):
                crt_path = os.path.join(OBJ_PATH, entry)
                if entry == 'gnathub':
                    NEW_GNATHUB_DIR_FOUND = True
                    NEW_GNATHUB_DIR_LEN = len(os.listdir(crt_path))
                elif entry == 'codepeer':
                    NEW_CODEPEER_DIR_FOUND = True
                    NEW_CODEPEER_DIR_LEN = len(os.listdir(crt_path))

        self.assertEqual(METRIX_FILES_COUNT, NEW_METRIX_FILES_COUNT,
            'All metrix results should be generated in /obj/test_metrix folder')
        self.assertEqual(GNATCHECK_FILES_COUNT, NEW_GNATCHECK_FILES_COUNT,
            'All expected gnatchek results should be generated in /obj/test_metrix folder')

        assert NEW_GNATHUB_DIR_FOUND, 'Codepeer directory should be in /obj/test_metrix folder'
        self.assertEqual(NEW_GNATHUB_DIR_LEN, GNATHUB_DIR_LEN,
                         'GNAThub directories should have same length')

        assert NEW_CODEPEER_DIR_FOUND, 'Codepeer directory should be in /obj/test_metrix folder'
        self.assertEqual(NEW_CODEPEER_DIR_LEN, CODEPEER_DIR_LEN,
                         'Codepeer directories should have same length')
