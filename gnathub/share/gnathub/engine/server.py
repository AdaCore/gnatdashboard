# GNAThub (GNATdashboard)
# Copyright (C) 2018, AdaCore
#
# This is free software;  you can redistribute it  and/or modify it  under
# terms of the  GNU General Public License as published  by the Free Soft-
# ware  Foundation;  either version 3,  or (at your option) any later ver-
# sion.  This software is distributed in the hope  that it will be useful,
# but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
# TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for  more details.  You should have  received  a copy of the GNU
# General  Public  License  distributed  with  this  software;   see  file
# COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy
# of the license.

"""GNAThub plug-in for launching the WebUI server.

"""
import GNAThub
from GNAThub import Plugin, Reporter

import SimpleHTTPServer
import SocketServer
import json
import re
import os
import socket

# Defining a default value for server port
DEFAULT_PORT = 8000

# The repository where .json files are supposed to be located
SERVER_DIR_PATH = GNAThub.html_data()


class My_Request_Handler(SimpleHTTPServer.SimpleHTTPRequestHandler):

    json_pattern = re.compile("^\/json\/[a-z]*.json")
    get_review_pattern = re.compile("^\/get-review\/")
    post_review_pattern = re.compile("^\/post-review\/")

    # Error if not declared ?
    def __base__():
        print ""

    def _error_not_found(self):
        # the headers
        self.send_response(404)
        self.send_header("Content-Type", "text/html")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write("<html><body>This url doesn't exist</body></html>")

    def _get_json(self):
        filename = self.path.replace('/json/', '')
        path = SERVER_DIR_PATH
        serverpath = os.path.join(os.getcwd(), path)
        filepath = ''

        # Find the path to the asked file
        for root, dirs, files in os.walk(serverpath):
            if filename in files:
                filepath = os.path.join(root, filename)

        if os.path.isfile(filepath):
            with open(filepath, 'r') as myFile:
                data = myFile.read()
                self.send_response(200)
                self.send_header("Content-Type", "application/json")
                self.send_header("Access-Control-Allow-Origin", "*")
                self.end_headers()
                self.wfile.write(json.dumps(data))
        else:
            self.send_response(404)
            self.send_header("Content-Type", "application/json")
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write("No such file")

    def _get_review(self):
        filename = 'codepeer_review.xml'

        self._export_codeper_bridge(filename)

        path = SERVER_DIR_PATH
        serverpath = os.path.join(os.getcwd(), path)
        filepath = ''

        # Find the path to the asked file
        for root, dirs, files in os.walk(serverpath):
            if filename in files:
                filepath = os.path.join(root, filename)

        if os.path.isfile(filepath):
            with open(filepath, 'r') as myFile:
                data = myFile.read()
                self.send_response(200)
                self.send_header("Content-Type", "text/xml")
                self.send_header("Access-Control-Allow-Origin", "*")
                self.end_headers()
                self.wfile.write(data)
        else:
            self.send_response(404)
            self.send_header("Content-Type", "application/json")
            self.send_header("Access-Control-Allow-Origin", "*")
            self.end_headers()
            self.wfile.write("No such file")

    def _export_codeper_bridge(self, filename):
        os.system('codepeer_bridge '
                  + '--output-dir=obj/codepeer/sdc.output/ '
                  + '--export-reviews=obj/gnathub/html-report/data/'
                  + filename)

    def _post_review(self):
        temp_filename = 'user_review_temp.xml'

        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)

        tempFile = open(temp_filename, "w+")
        tempFile.write(post_data)
        tempFile.close()

        self._import_codepeer_bridge(temp_filename)

        self.send_response(200)
        self.send_header("Content-Type", "text/xml")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write("OK")

    def _import_codepeer_bridge(self, filename):
        os.system('codepeer_bridge '
                  + '--output-dir=obj/codepeer/sdc.output/ '
                  + '--import-reviews='
                  + filename)

    def do_GET(self, **args):
        print "received the following GET request: {}".format(self.path)

        if self.json_pattern.match(self.path):
            self._get_json()
        elif self.get_review_pattern.match(self.path):
            self._get_review()
        elif self.post_review_pattern.match(self.path):
            self._post_review()
        else:
            self._error_not_found()

    def do_POST(self, **args):
        print "received the following POST request: {}".format(self.path)

        if self.post_review_pattern.match(self.path):
            self._post_review()
        else:
            self._error_not_found()


class Launch_Server(Plugin, Reporter):

    @property
    def name(self):
        return 'server'

    def __init__(self):
        super(Launch_Server, self).__init__()

    def launch_server(self):

        # Find the first open port to launch the server
        # or take the one given by the user
        port = DEFAULT_PORT
        # port = self.find_free_port()

        if GNAThub.port():
            port = GNAThub.port()

        httpd = SocketServer.TCPServer(("", port), My_Request_Handler)
        print("Launched GNAThub server on port {}".format(port))

        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            httpd.shutdown()

    def find_free_port(self):
        tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        tcp.bind(('', 0))
        addr, port = tcp.getsockname()
        tcp.close()
        return port

    def report(self):
        return GNAThub.EXEC_SUCCESS


# Script entry point
if __name__ == '__main__':
    Launch_Server().launch_server()
