#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

import os
import sys
from optparse import OptionParser, OptionGroup
import urlparse
import urllib

from couchdbkit import Server
from couchdbkit.loaders import FileSystemDocsLoader


def parse_uri(string):
    parts = urlparse.urlsplit(urllib.unquote(string))
    if parts[0] != 'http' and parts[0] != 'https':
        raise ValueError('Invalid dbstring')
     
    path = parts[2].strip('/').split('/')

    dbname = ''
    docid = ''
    if len(path) >= 1:
        db_parts=[]
        i = 0
        while 1:
            try:
                p = path[i]
            except IndexError:
                break

            if p == '_design': break
            db_parts.append(p)
            i = i + 1
        dbname = '/'.join(db_parts)
        
        if i < len(path) - 1:
            docid = '/'.join(path[i:])

    server_uri = '%s://%s' % (parts[0], parts[1])
    return server_uri, dbname, docid


def parse_auth(string):
    """ get username and password for an url string """
    parts = urlparse.urlsplit(urllib.unquote(string))
    
    server_parts = parts[1].split('@')
    if ":" in server_parts[0]:
        username, password = server_parts[0].split(":")
    else:
        username = server_parts[0]
        password = ''
    server_uri = "%s://%s" % (parts[0], server_parts[1])
    return username, password, server_uri

def setup(dbstring, path):
    server_uri, db_name, docid = parse_uri(dbstring)
    if "@" in server_uri:
        username, password, server_uri = parse_auth(server_uri) 
        server = Server(server_uri)
        server.res.add_authorization(httpc.BasicAuth((uri.username, uri.password)))
    else:
        server = Server(server_uri)
    db = server.get_or_create_db("couchdbproxy")
    loader = FileSystemDocsLoader(path)
    loader.sync(db, verbose=True)
    
    
def main():
    parser = OptionParser(version="%prog 0.1", usage="""
./setup.py setup server_uri path

Command line to setup initial couchapp on couchdbproxy. 
it send a design doc compatible with couchapp.

Example :
    ./setup.py http://127.0.0.1:5984 ../couchapps/couchdbproxy""")
    
    options, args = parser.parse_args()
    default_path =  os.path.normpath(os.path.join(os.path.dirname(__file__), "../couchapps"))
    if len(args) == 0:
        setup('http://127.0.0.1:5984', default_path)
    elif len(args) == 1:
        setup(args[0], default_path)
    elif len(args) == 2:
        if args[1].startswith("."):
            path = os.path.normpath(os.path.join(os.getcwd(), args[1]))
        else:
            path = os.path.normpath(args[1])
        setup(args[0], path)

if __name__ == "__main__":
    main()