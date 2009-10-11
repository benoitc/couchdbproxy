# couchdbproxy

Simple multinode couchdb proxy. Allow you to proxy from one ip to muliple couchdb nodes running on differents ips or ports. Main objectiv is to have something like 

    http://someuser.yourdomain.com -> someip:someport

CouchdbProxy support some basic url rewriting and domain aliasing.

## Installation :

1. requirements


  - bind 9 or ant dns server that handle wildcards in ANAME
  - couchdb 0.10x or sup
  - python couchdbkit for console utilities & basic setup. 


2. Configure bind

Add your our zone. Here is an example to configure a local zone for development though configuration is the same for production server, you just have to change the domain name.

in named.conf add :

    zone "couchdbproxy.dev" {
            type master;
            file "couchdbproxy.dev.zone";
            allow-transfer {localhost; };
    };


then add a zone. On my machine it's in /var/named. Create a file couchdbproxy.dev.zone:

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

3. Configure master couchdb node.

This node will be used to store and retrieve dynamically nodes for one user and cname. Install CouchDB somewhere by following [CouchDB insyallation](http://wiki.apache.org/couchdb/Installation) and set proxy.conf in couchdbproxy/priv folder :


    % base hostname used for rewriting
    {hostname, "couchdbproxy.dev"}.
    {port, 8000}.

    % params of couchdb node used to maintain connections user-couchdb
    {proxy_hostconfig, [{host, "127.0.0.1"}, {port, 5984}]}.
    {proxy_db, "couchdbproxy"}.
    
`hostname` is the basename used to decte cname and user urls. `port` is the port on which you want to run couchdbproxy.
    
if you want to add an admin username/password do, change proxy_hostconfig accordingly:

    {proxy_hostconfig, [{host, "127.0.0.1"}, {port, 5984}, {username, "someuser"}, {password, "somepassword"}]}.
    
    
4. build couchdbproxy and setup the basic design doc :

In couchdbproxy source folder run :

    make && make setup

That's it.


## Play with couchdbproxy

You can add a node, username, alias dynamically thanks to the `couchdproxy` script in `scripts` folder. 


### basic usage 

The first thing you have to do is to setup an node. A node is a machine with an ip. You will launch couchdb user nodes on this machine on different ports. To add a node on localhost run :

    ./couchdbproxy.py add_node http://127.0.0.1:5984 node0 127.0.0.1
    
Then you want to set a user couchdb node on node0 machine :

    ./couchdbproxy.py add_user http://127.0.0.1:5984 node0 benoitc 5985
    
Then connect on `http://benoitc.couchdbproxy.dev:8000` and you will fall in benoitc node.

To remove this user :

    ./couchdbproxy.py remove_user http://127.0.0.1:5984 benoitc
    
Get more usage by running command `./couchdbproxy.py --help`

### some cool features

There are cool url rewriting offered by couchdbproxy that allow yu to access easily on a database or a couchapp. Ex :

if `benoitc` user has a database `blog` on its node, you can access it with the url `http://blog.benoitc.couchdbproxy.dev:8000`. If now has put in this db a couchapp named `myblog` too, yo can also access to it with the url `http://myblog.blog.benoitc.couchdbproxy.dev:8000` .


Now imagine you have a domain name call `benoitc.local` and want to access to the the couchapp myblog in blog db from benoitc's couchdb node. You could do it easyly by pointing benoitc.local to your couchdbproxy IP and add an alias :

    ./couchdbproxy.py add_alias http://127.0.0.1:5984 node0 benoitc.local 5985 /blog/_design/myblog
    
easy.

## Todo :

- share http connection.
- more url rewriting
- add more dispatching possibilities
- ...




