# CouchDBProxy

Simple multinode couchdb proxy. It allows you to proxy from one IP address to
multiple couchdb nodes running on different IPs or ports. CouchDBProxy also
supports streaming, some basic url rewriting and domain aliasing. You could
add/remove a *user couchdb node* or a *domain alias* dynamically. So basically
it allows you to host a little `couch.io` like on your own site.

With CouchDBProxy you could do something like

    http://someuser.yourdomain.com -> someip:someport
    
or 

    http://mydomain.com - someip:someport/mydb/_design/mycouchapp
    http://someuser.yourdomain.com -> someip:someport

CouchDBProxy uses [couchbeam](http://bitbucket.org/benoitc/couchbeam/) to dialog
with CouchDB and [mochiweb](http://code.google.com/p/mochiweb/) for HTTP
syntax-management.


## Installation

*Users of 0.2 you can migrate your data to the new api by running
./scripts/migrate01.py. It will update your views and migrate data.*

### 1. Requirements

  - bind 9 or any dnDNSs server that handle wildcards in ANAME. -
  - [CouchDB](http://couchdb.apache.org) 0.10.x or newer.
  - [Python CouchdbKit](http://bitbucket.org/benoitc/couchdbkit/) for console utilities & basic setup.


### 2. Configure Bind

Add your our zone. Here is an example to configure a local zone for development
though configuration is the same for production server, you just have to change
the domain name.

in named.conf add:

    zone "CouchDBProxy.dev" {
            type master;
            file "CouchDBProxy.dev.zone";
            allow-transfer { localhost; };
    };


then add a zone. On my machine it's in /var/named. Create a file
couchdbproxy.dev.zone:

    $TTL    86400
    @       IN      SOA     ns.couchdbproxy.dev. root.couchdbproxy.dev.  (
                                          2009091111 ; Serial
                                          10800       ; Refresh
                                                                              3600        ; Retry
                                                                              3600000     ; Expire
                                                                              86400 )     ; Minimum

                  IN      NS      ns0.couchdbproxy.dev.

                  IN      A       127.0.0.1
    ns0           IN      A       127.0.0.1
    *             IN      A       127.0.0.1

### 3. Configure Master Couchdb Node.

This node will be used to store and retrieve dynamically nodes for one user and
cname. Install CouchDB somewhere by following [CouchDB installation](http://wiki.apache.org/couchdb/Installation) and set your en configuration file. All configuration is set in couchdbproxy application environment at startup. One full configuration file is in config:

    {couchdbproxy_hostname, "couchdbproxy.dev"}. % base hostname used for rewriting
    {couchdbproxy_name, couchdbproxy}. % erlang node name
    {couchdbproxy_port, 8000}. % port on which couchdbproxy listen
    {couchdbproxy_ip, "0.0.0.0"}. % ip of couchdbproxy
    {couchdbproxy_cookie, 'couchdbproxy_cookie_default'}. % erlang cookiue
    {couchbeam_heart_command, "start.sh"}. % heartbeat command

    % params of couchdb node used to maintain connections user-couchdb
    {couchdbproxy_couchdb_params, [{host, "127.0.0.1"}, {port, 5984}]}. % parameters of master couchdb 
    {couchdbproxy_db, "couchdbproxy"}. % couchdb database
    
`couchdbproxy_hostname` is the basename used to detect CNAME and user urls. `couchdbproxy_port` is the port on which you want to run CouchDBProxy.
    
if you want to add an admin username/password do, change `couchdbproxy_couchdb_params` accordingly:

    {proxy_hostconfig, [{host, "127.0.0.1"}, {port, 5984}, {username, "someuser"}, {password, "somepassword"}]}.
    
    
### 4. Build CouchDBProxy and Setup the Basic Design Doc:

Install couchdbkit:

    easy_install -U couchdbkit

Then, in CouchDBProxy source folder run:

    make && make setup

That's it.


## Play with CouchDBProxy

You can add a node, username, alias dynamically thanks to the `couchdproxy`
script in `scripts` folder.

### run couchdbproxy
  
At development run `start-dev.sh` script:
    
    ./start-dev.sh config/couchdbproxy.erlenv
    
Or in production you could run `start.sh` script:

    ./start.sh config/couchdbproxy.erlenv

### basic usage 

The first thing you have to do is to setup a machine You can launch CouchDB
nodes on this machine on different ports. To add a machine on localhost run:

    ./couchdbproxy.py add_machine http://127.0.0.1:5984 m0 127.0.0.1
    
Then you want to set a couchdb node on m0 machine:

    ./couchdbproxy.py add_node http://127.0.0.1:5984 m0 benoitc 5985
    
    Connect on `http://benoitc.couchdbproxy.dev:8000` and you will fall in
    benoitc node.

To remove this node :

    ./couchdbproxy.py remove_node http://127.0.0.1:5984 benoitc
    
Get more usage by running command `./couchdbproxy.py --help`

### Cool Features

There are cool url rewriting offered by CouchDBProxy that allows you to access
easily on a database or a couchapp. For example:

If the user `benoitc` has a database `blog` on its node, you can access it with
the url `http://blog.benoitc.CouchDBProxy.dev:8000`. If there is a couchapp in
this database named `myblog`, you can also access to it with the url
`http://myblog.blog.benoitc.CouchDBProxy.dev:8000`.

Now imagine, you have a domain name called `benoitc.local` and want to access to
the the couchapp `myblog` in `blog` db from benoitc's couchdb node. You could do
it easily by pointing `benoitc.local` to your node and add an alias:

    ./couchdbproxy.py add_alias http://127.0.0.1:5984 benoitc benoitc.local /blog/_design/myblog
    
Easy!

## Todo:

- caching with redis/memcached
- add more dispatching possibilities
- ...

