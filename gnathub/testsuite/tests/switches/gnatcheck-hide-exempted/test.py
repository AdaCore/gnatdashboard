"""Check that object folder is changed and all files have been created."""

import os.path

from unittest import TestCase
from support.mock import GNAThub, Project

class TestSimpleWithHideExemptedExample(TestCase):
    def setUp(self):
        self.longMessage = True

    def testGNATcheckHideExempted(self):
        PROJECT = Project.simple_with_hide_exempted()

        # Define 'obj' folder name and path
        OBJ_FOLDER = 'obj'
        OBJ_PATH = os.path.join(PROJECT.install_dir, OBJ_FOLDER)

        # 1st step: Run GNAThub without '--gnatcheck-hide-exempted' switch
        assert not os.path.exists(OBJ_PATH), 'Object folder should not exist'
        gnathub = GNAThub(PROJECT, plugins=['gnatcheck'])

        assert os.path.exists(OBJ_PATH), 'Object folder should exist'

        # Check that results are generated in /obj
        GNATCHECK_FILES_COUNT = 0
        GNATCHECK_FILES_FOUND = False

        GNATHUB_DIR_FOUND = False
        GNATHUB_DIR_LEN = 0

        for entry in os.listdir(OBJ_PATH):
            if entry.startswith('gnatcheck') and entry.endswith('.out'):
                GNATCHECK_FILES_COUNT += 1
            elif os.path.isdir(os.path.join(OBJ_PATH, entry)):
                crt_path = os.path.join(OBJ_PATH, entry)
                if entry == 'gnathub':
                    GNATHUB_DIR_FOUND = True
                    GNATHUB_DIR_LEN = len(os.listdir(crt_path))

        GNATCHECK_FILE_FOUND = GNATCHECK_FILES_COUNT != 0
        assert GNATCHECK_FILE_FOUND, 'GNATcheck files should be generated in /obj folder'
        assert GNATHUB_DIR_FOUND, 'GNAThub directory should be generated in /obj folder'

        # Check DB content after gnatcheck run
        gnathub.run(script='check-db-gnatcheck.py')

        # 2nd step: Run GNAThub with '--gnatcheck-hide-exempted' switch
        gnathub = GNAThub(PROJECT, plugins=['gnatcheck'], gnatcheck_hide_exempted=True)

        GNATHUB_DIR_FOUND = False
        GNATHUB_DIR_LEN = 0

        for entry in os.listdir(OBJ_PATH):
            if entry.startswith('gnatcheck') and entry.endswith('.out'):
                GNATCHECK_FILES_COUNT += 1
            elif os.path.isdir(os.path.join(OBJ_PATH, entry)):
                crt_path = os.path.join(OBJ_PATH, entry)
                if entry == 'gnathub':
                    GNATHUB_DIR_FOUND = True
                    GNATHUB_DIR_LEN = len(os.listdir(crt_path))
                elif entry == 'codepeer':
                    CODEPEER_DIR_FOUND = True
                    CODEPEER_DIR_LEN = len(os.listdir(crt_path))

        GNATCHECK_FILE_FOUND = GNATCHECK_FILES_COUNT != 0
        assert GNATCHECK_FILE_FOUND, 'GNATcheck files should be generated in /obj folder'
        assert GNATHUB_DIR_FOUND, 'GNAThub directory should be generated in /obj folder'

        # Check DB content after gnatcheck run with '--gnatcheck-hide-exempted' switch
        gnathub.run(script='check-db-gnatcheck-hide.py')
