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

import thread
import posixpath
import urllib

from SimpleHTTPServer import SimpleHTTPRequestHandler
from BaseHTTPServer import HTTPServer

# Defining a default value for client server port
DEFAULT_PORT = 8080

# The repository where .json files are supposed to be located
SERVER_DIR_PATH = GNAThub.html_data()

# The info for the logs
GNATHUB_LOG = GNAThub.logs()
API_SERVER_LOG = os.path.join(GNATHUB_LOG, "webui_api_server.log")
CLIENT_SERVER_LOG = os.path.join(GNATHUB_LOG, "webui_client_server.log")


class My_Request_Handler(SimpleHTTPServer.SimpleHTTPRequestHandler):

    json_pattern = re.compile("^\/json\/[a-z]*.json")
    get_review_pattern = re.compile("^\/get-review\/")
    post_review_pattern = re.compile("^\/post-review\/")

    # Error if not declared ?
    def __base__():
        print ""

    def log_message(self, format, *args):
        with open(API_SERVER_LOG, 'a') as file_descriptor:
            file_descriptor.write("%s - - [%s] %s\n" %
                                  (self.client_address[0],
                                   self.log_date_time_string(),
                                   format % args))

    def _log_api_error(self, error):
        print "[ERROR] {}".format(error)
        with open(CLIENT_SERVER_LOG, 'a') as file_descriptor:
            msg = "[ERROR] " + error
            file_descriptor.write(msg)

    def _error_not_found(self):
        # the headers
        self.send_response(404)
        self.send_header("Content-Type", "text/html")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write("<html><body>This url doesn't exist</body></html>")
        self._log_api_error(self.path + " : url doesn't exist")

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
            self._log_api_error(filepath + " : file doesn't exist")

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
            self._log_api_error(filepath + " : file doesn't exist")

    def _export_codeper_bridge(self, filename):
        cmd = ('codepeer_bridge'
               + ' --output-dir=' + GNAThub.output_dir()
               + ' --db-dir=' + GNAThub.db_dir()
               + ' --export-reviews=obj/gnathub/html-report/data/'
               + filename
               + ' >> ' + API_SERVER_LOG + ' 2>&1')
        os.system(cmd)

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
        cmd = ('codepeer_bridge'
               + ' --output-dir=' + GNAThub.output_dir()
               + ' --db-dir=' + GNAThub.db_dir()
               + ' --import-reviews='
               + filename
               + ' >> ' + API_SERVER_LOG + ' 2>&1')
        os.system(cmd)

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

        port = DEFAULT_PORT

        if GNAThub.port():
            port = GNAThub.port()

        api_port = port + 1

        print("Launched GNAThub API server on port {}".format(api_port))
        thread.start_new_thread(self.launch_api_server, (api_port,))
        print("Launched GNAThub client server on port {}".format(port))
        self.set_client_server(port)

    def set_client_server(self, port):
        launch_client_server(port)

    def launch_api_server(self, api_port):

        # Define the server for the Web API
        httpd_api = SocketServer.TCPServer(("", api_port), My_Request_Handler)

        try:
            httpd_api.serve_forever()
        except KeyboardInterrupt:
            httpd_api.shutdown()

    def find_free_port(self):
        tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        tcp.bind(('', 0))
        addr, port = tcp.getsockname()
        tcp.close()
        return port

    def report(self):
        return GNAThub.EXEC_SUCCESS


class RootedHTTPServer(HTTPServer):

    def __init__(self, base_path, *args, **kwargs):
        HTTPServer.__init__(self, *args, **kwargs)
        self.RequestHandlerClass.base_path = base_path


class RootedHTTPRequestHandler(SimpleHTTPRequestHandler):

    def log_message(self, format, *args):
        with open(CLIENT_SERVER_LOG, 'a') as file_descriptor:
            file_descriptor.write("%s - - [%s] %s\n" %
                                  (self.client_address[0],
                                   self.log_date_time_string(),
                                   format % args))

    def translate_path(self, path):
        path = posixpath.normpath(urllib.unquote(path))
        words = path.split('/')
        words = filter(None, words)
        path = self.base_path
        for word in words:
            drive, word = os.path.splitdrive(word)
            head, word = os.path.split(word)
            if word in (os.curdir, os.pardir):
                continue
            path = os.path.join(path, word)
        return path


def launch_client_server(port, HandlerClass=RootedHTTPRequestHandler,
                         ServerClass=RootedHTTPServer):

    server_address = ('', port)
    httpd = ServerClass('obj/gnathub/html-report', server_address,
                        HandlerClass)
    sa = httpd.socket.getsockname()
    print "Serving HTTP on ", sa[0], "port", sa[1], "..."

    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        httpd.shutdown()


# Script entry point
if __name__ == '__main__':
    Launch_Server().launch_server()
