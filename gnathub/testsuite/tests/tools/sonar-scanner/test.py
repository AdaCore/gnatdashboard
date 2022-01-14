"""Check that all files have been created."""

import os
import time

import json, requests, pprint
from requests.adapters import HTTPAdapter
from requests.packages.urllib3.util.retry import Retry

from configparser import ConfigParser

from unittest import TestCase
from support.mock import GNAThub, Project, Script

class TestSonarScannerSupport(TestCase):
    def setUp(self):
        self.longMessage = True
        plugins = ['gnatcheck']

        if os.environ["WITH_SONAR"]:
            plugins = ['gnatcheck', 'sonar-config', 'sonar-scanner']
            str_args = {'sonar-scanner': ['-Dsonar.login=admin', '-Dsonar.password=admin']}
            self.gnathub = GNAThub(
                Project.simple_sonar(), plugins=plugins, tool_args=str_args)
        else:
            self.gnathub = GNAThub(Project.simple_sonar(), plugins=plugins)

    def testDatabaseContent(self):
        script_output_file = os.path.abspath('script.out')
        self.gnathub.run(script=Script.db2cfg(), output=script_output_file)

        parser = ConfigParser()
        parser.optionxform = str

        parser.read(script_output_file)
        self.assertEqual(len(parser.sections()), 27)

        if os.environ["WITH_SONAR"]:            
            user = 'admin'
            pwd = 'admin'

            def check_Ada_language():
                ''' Check if Ada language is supported
                '''
                SONAR_LANGUAGES_URL = 'http://localhost:9000/api/languages/list'

                session = requests.Session()
                session.auth = user, pwd
                call = getattr(session, 'get')
                response = call(SONAR_LANGUAGES_URL)

                self.assertTrue(response.status_code < 300,
                                'Expected < 300 return HTT response (OK).' +
                                'Got %s' % str(response.status_code))
                binary = response.content
                output = json.loads(binary)
                # pprint.pprint(output)

                ADA_LANG = False
                for record in output['languages']:
                    if record['key'] == 'ada' and record['name'] == 'Ada':
                        ADA_LANG = True
                        break
                    else:
                        continue

                self.assertTrue(ADA_LANG, 'Ada language is not supported')
                session.close()

            def check_Ada_quality_profile():
                ''' Check if Ada quality profile is supported
                '''
                SONAR_QP_URL = 'http://localhost:9000/api/qualityprofiles/search'

                session = requests.Session()
                session.auth = user, pwd
                call = getattr(session, 'get')
                response = call(SONAR_QP_URL)

                self.assertTrue(response.status_code < 300,
                                'Expected < 300 return HTT response (OK).' +
                                'Got %s' % str(response.status_code))
                binary = response.content
                output = json.loads(binary)
                # pprint.pprint(output)

                GNATDASHBOARD_WAY = False
                LANGUAGE_ADA = False
                ACTIVE_RULES_COUNT = 0
                for record in output['profiles']:
                    if 'name' in record and record['name'] == 'GNATdashboard way':
                        GNATDASHBOARD_WAY = True
                        if 'language' in record and record['language'] == 'ada':
                            LANGUAGE_ADA = (record['languageName'] == 'Ada')
                            ACTIVE_RULES_COUNT = record['activeRuleCount']
                    else:
                       continue

                self.assertTrue(GNATDASHBOARD_WAY,
                                'missing GNATdashboard way from Quality Profile')
                self.assertEqual(ACTIVE_RULES_COUNT, 476,
                                 'unexpected value for Ada active rules.' +
                                 ' Found "%s" instead of 476' % str(ACTIVE_RULES_COUNT))
                session.close()

            def check_Ada_project_uploaded():
                ''' Check if Ada project is uploaded
                The exected uploaded project name is 'Simple_Sonar'
                '''
                SONAR_SEARCH_URL = 'http://localhost:9000/api/projects/search'
                EXPECTED_NAME = 'Simple_Sonar'

                session = requests.Session()
                session.auth = user, pwd
                call = getattr(session, 'get')
                response = call(SONAR_SEARCH_URL)

                self.assertTrue(response.status_code < 300,
                                'Expected < 300 return HTT response (OK).' +
                                'Got %s' % str(response.status_code))
                binary = response.content
                output = json.loads(binary)
                # pprint.pprint(output)

                for record in output['components']:
                    self.assertTrue(
                        'key' in record,
                        'missing entry for name in record %s' % str(record))
                    self.assertEqual(record['key'], EXPECTED_NAME,
                        'unexpected value for "%s"' % record['key'])
                    self.assertTrue('name' in record,
                        'missing entry for name in record %s' % str(record))
                    self.assertEqual(record['name'], EXPECTED_NAME,
                        'unexpected value for "%s"' % record['name'])
                session.close()

            def check_Ada_project_analysis():
                ''' Check if Ada project has expected number of code_smells
                    For the dashboard project 'Simple_Sonar' 17 code smells are expected

                    The request should be
                       http://localhost:9000/api/measures/component?
                         component=Simple_Sonar&metricKeys=reliability_rating,bugs,code_smells
                    The expected response is

                       {"component":{"id":"AXfP0Sfu2OokptDLjqm9",
                            "key":"Simple_Sonar",
                            "name":"Simple_Sonar",
                            "qualifier":"TRK",
                            "measures":[{"metric":"reliability_rating", "value":"1.0","bestValue":true},
                                        {"metric":"bugs","value":"0","bestValue":true},
                                        {"metric":"code_smells","value":"34","bestValue":false}]}}
                '''
                PROJECT_KEY = 'Simple_Sonar'
                METRICS_LIST = ['reliability_rating', 'code_smells', 'bugs']
                RQ_PARAMS = {'component': PROJECT_KEY, 'metricKeys': ','.join(METRICS_LIST)}
                SONAR_MEASURES_URL = 'http://localhost:9000/api/measures/component'

                session = requests.Session()
                session.auth = user, pwd
                call = getattr(session, 'get')

                # Sleep for 30 seconds to be sure that the analysis results are uploaded
                time.sleep(30)

                response = call(SONAR_MEASURES_URL, params=RQ_PARAMS)
                self.assertTrue(response.status_code < 300,
                                'Expected < 300 return HTT response (OK).' +
                                'Got %s' % str(response.status_code))

                print (response.url)
                print (response.text)

                json_data = response.json() if response and response.status_code == 200 else None
                self.assertTrue(json_data and 'component' in json_data,
                                'unexpected value for returned data')
                self.assertTrue('measures' in json_data['component'],
                                'unexpected value for returned data')
                self.assertTrue(len(json_data['component']['measures']) != 0,
                                'no metric is found in measures')

                # Gather the informations from the response
                reliability_rating = 0.0
                bugs = 0
                code_smells = 0
                for record in json_data['component']['measures']:
                    if record['metric'] == 'reliability_rating':
                        reliability_rating = record['value']

                    if record['metric'] == 'bugs':
                        bugs = int(record['value'])

                    if record['metric'] == 'code_smells':
                        code_smells = int(record['value'])

                self.assertEqual(bugs, 0, 'Not expected metric %s' % str(bugs))
                self.assertEqual(code_smells, 34, 'Not expected metric %s' % str(code_smells))
                session.close()

            # Check if the Sonar Ada plugin is used
            # 1. Check if the Ada language is supported
            check_Ada_language()

            # 2. Check if the Ada quality profile is present
            check_Ada_quality_profile()

            # 3. Check if the uploaded project name is 'Simple_Sonar'
            check_Ada_project_uploaded()

            # 4. Check if the expected metrics are uploaded for 'Simple_Sonar'
            check_Ada_project_analysis()
