"""Check that object folder is changed and all files have been created."""

import os.path

from unittest import TestCase
from support.mock import GNAThub, Project

class TestSimpleExample(TestCase):
    def setUp(self):
        self.longMessage = True

    def testIncrementalSwitch(self):
        PROJECT = Project.simple()

        # Define 'obj' folder name and path
        OBJ_FOLDER = 'obj'
        OBJ_PATH = os.path.join(PROJECT.install_dir, OBJ_FOLDER)

        # 1st step: Run GNAThub without '--incremental' switch
        assert not os.path.exists(OBJ_PATH), 'Object folder should not exist'
        gnathub = GNAThub(PROJECT, plugins=['codepeer'])

        assert os.path.exists(OBJ_PATH), 'Object folder should exist'

        # Check that results are generated in /obj
        METRIX_FILES_COUNT = 0
        METRIX_FILES_FOUND = False

        GNATHUB_DIR_FOUND = False
        GNATHUB_DIR_LEN = 0

        CODEPEER_DIR_FOUND = False
        CODEPEER_DIR_LEN = 0

        for entry in os.listdir(OBJ_PATH):
            if entry.endswith('.metrix') or entry.startswith('metrix'):
                METRIX_FILES_COUNT += 1
            elif os.path.isdir(os.path.join(OBJ_PATH, entry)):
                crt_path = os.path.join(OBJ_PATH, entry)
                if entry == 'gnathub':
                    GNATHUB_DIR_FOUND = True
                    GNATHUB_DIR_LEN = len(os.listdir(crt_path))
                elif entry == 'codepeer':
                    CODEPEER_DIR_FOUND = True
                    CODEPEER_DIR_LEN = len(os.listdir(crt_path))

        METRIX_FILE_FOUND = METRIX_FILES_COUNT != 0
        assert not METRIX_FILE_FOUND, 'No metrix should be generated in /obj folder'
        assert GNATHUB_DIR_FOUND, 'GNAThub directory should be generated in /obj folder'
        assert CODEPEER_DIR_FOUND, 'Codepeer directory should be generated in /obj folder'

        # Check DB content
        gnathub.run(script='check-db-codepeer.py')

        # 2nd step: Run GNAThub without '--incremental' switch
        gnathub = GNAThub(PROJECT, plugins=['gnatmetric'])

        GNATHUB_DIR_FOUND = False
        GNATHUB_DIR_LEN = 0

        CODEPEER_DIR_FOUND = False
        CODEPEER_DIR_LEN = 0

        for entry in os.listdir(OBJ_PATH):
            if entry.endswith('.metrix') or entry.startswith('metrix'):
                METRIX_FILES_COUNT += 1
            elif os.path.isdir(os.path.join(OBJ_PATH, entry)):
                crt_path = os.path.join(OBJ_PATH, entry)
                if entry == 'gnathub':
                    GNATHUB_DIR_FOUND = True
                    GNATHUB_DIR_LEN = len(os.listdir(crt_path))
                elif entry == 'codepeer':
                    CODEPEER_DIR_FOUND = True
                    CODEPEER_DIR_LEN = len(os.listdir(crt_path))

        METRIX_FILE_FOUND = METRIX_FILES_COUNT != 0
        assert METRIX_FILE_FOUND, 'Metrix should be generated in /obj folder'
        assert GNATHUB_DIR_FOUND, 'GNAThub directory should be generated in /obj folder'
        assert CODEPEER_DIR_FOUND, 'Codepeer directory should be generated in /obj folder'

        # Check DB content after gnatmetric run
        gnathub.run(script='check-db-metrics.py')

        # 3rd step: Run GNAThub with '--incremental' switch
        gnathub = GNAThub(PROJECT, plugins=['codepeer'], incremental=True)

        # Check DB content after gnatmetric run
        gnathub.run(script='check-db-incremental.py')

        # 4th step: Run GNAThub with '--incremental' switch
        gnathub = GNAThub(PROJECT, plugins=['codepeer'], incremental=True)

        # Check DB content after gnatmetric run
        gnathub.run(script='check-db-incremental.py')

        # 5th step: Run GNAThub with '--incremental' switch
        gnathub = GNAThub(PROJECT, plugins=['gnatmetric'], incremental=True)

        # Check DB content after gnatmetric run
        gnathub.run(script='check-db-incremental.py')
