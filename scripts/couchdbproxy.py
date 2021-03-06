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
from restkit import httpc


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


class CouchdbProxyCtl(object):
    
    def __init__(self, server_uri):
        server_uri, db_name, docid = parse_uri(server_uri)
        if "@" in server_uri:
            username, password, server_uri = parse_auth(server_uri) 
            self.server = Server(server_uri)
            self.server.res.add_authorization(httpc.BasicAuth((uri.username, uri.password)))
        else:
            self.server = Server(server_uri)
        self.db = self.server.get_or_create_db("couchdbproxy")
            
    def add_machine(self, name, ip):
        doc = {
            "name": name,
            "ips": ip,
            "type": "machine"
        }
        
        res = self.db.view("couchdbproxy/machines_byname", key=name).one()
        if res:
            doc.update({
                '_id': res['value']['_id'],
                '_rev': res['value']['_rev']
            })
        
        self.db.save_doc(doc)
        
    def remove_machine(self, name):
        res = self.db.view("couchdbproxy/machines_byname", key=name).one()
        if res:
            del self.db[res['id']]
            return 
        print "notfound"
        
    def add_node(self, machine, nodename, port):
        doc = {
            "nodename": nodename,
            "machine": machine,
            "port": int(port),
            "type": "node",
            "active": True
        }
        
        res = self.db.view("couchdbproxy/nodes", key=nodename).one()
        if res:
            doc.update({
                '_id': res['value']['_id'],
                '_rev': res['value']['_rev']
            })
        self.db.save_doc(doc)
        
    def remove_node(self, nodename):
        res = self.db.view("couchdbproxy/nodes", key=nodename).one()
        if res:
            del self.db[res['id']]
            return 
        print "notfound"

        
    def add_alias(self, nodename, hostname, path="/"):
        doc = {
            "nodename": nodename,
            "hostname": hostname,
            "path": path,
            "type": "alias"
        }
        
        res = self.db.view("couchdbproxy/alias", key=hostname.split('.')).one()
        if res:
            doc.update({
                '_id': res['value']['_id'],
                '_rev': res['value']['_rev']
            })
        self.db.save_doc(doc)

    def remove_alias(self, hostname):
        res = self.db.view("couchdbproxy/alias", key=hostname.split('.')).one()
        if res:
            del self.db[res['id']]
            return
        print "notfound"
        
        
def main():
    parser = OptionParser(version="%prog 0.1", usage="""
%prog cmd  

Command line utility to manage couchdbproxy.

- Add a machine :
    
    couchdbproxy add_machine proxy_url name someip
    
    proxy_url is url of the proxy couchdb node which maintain list of aliases, machines & nodes.
    machine is a string without spaces in ascii. It is the name you will give to a machine (vm or real hardware)
    ip is on the form 255.255.255.255

- Remove machine :

    couchdbproxy remove_machine proxy_url name
    
- Add/update a CouchDB node :

    couchdbproxy add_node proxy_url machinename nodename port
    
    port is an integer and corrspond to installation. example: if you set a machine 
    "mymachine" to ip "127.0.0.1" and create a couchdb node on port 5985 :
    
         couchdbproxy add_node http://127.0.0.1:5984 mymachine somenodename 5985
    
- remove a CouchDB node : 

    couchdbproxy remove_node proxy_url nodename
    
    
- Add/update alias

    couchdbproxy add_alias proxy_url nodename hostname path
    

    alias are a simple way to rewrite dbs, couchapp paths. By default 
    
    nameofdesigndoc.dbname.username.example.com 

    (every combinations are possible). But you could add any host alias or some parts 
    of the path as alias. If path is omitted it is set to "/".
    
    example : 
    
    alias mywebsite.com to the db testdb  in node "mynode":
    
    couchdbproxy add_alias http://127.0.0.1:5984 mynode mywebsite.com /testdb
    

- Remove alias

    couchdbproxy remove_alias proxy_url alias
    
    example :
    
    couchdbproxy remove_alias http://127.0.0.1:5984 mywebsite.com
    
""")
    options, args = parser.parse_args()
    if len(args) < 1:
        return parser.error('incorrect number of arguments')
        
    if args[0] == "add_machine":
        if len(args) < 4:
            return parser.error('incorrect number of arguments.')
        proxy = CouchdbProxyCtl(args[1])
        proxy.add_machine(args[2], args[3])
    elif args[0] == "remove_machine":
        if len(args) < 3:
            return parser.error('incorrect number of arguments.')
        proxy = CouchdbProxyCtl(args[1])
        proxy.remove_machine(args[2])
    elif args[0] == "add_node" or args[0] == "update_node":
        if len(args) < 5:
            return parser.error('incorrect number of arguments.')
        proxy = CouchdbProxyCtl(args[1])
        proxy.add_node(args[2], args[3], args[4])
    elif args[0] == "remove_node":
        if len(args) < 3:
            return parser.error('incorrect number of arguments.')
        proxy = CouchdbProxyCtl(args[1])
        proxy.remove_node(args[2])
    elif args[0] == "add_alias" or args[0] == "update_alias":
        if len(args) < 4:
            return parser.error('incorrect number of arguments.')
        proxy = CouchdbProxyCtl(args[1])
        if len(args) == 5:
            proxy.add_alias(args[2], args[3], args[4])
        else:
            proxy.add_alias(args[2], args[3])
    elif args[0] == "remove_alias":
        if len(args) < 3:
            return parser.error('incorrect number of arguments.')
        proxy = CouchdbProxyCtl(args[1])
        proxy.remove_alias(args[2])
    else:
        print "%s is an unknown command, sorry." % args[0]

if __name__ == "__main__":
    main()