{application, couchdbproxy,
 [{description, "couchdbproxy"},
  {vsn, "0.1"},
  {modules, [
    couchdbproxy,
    couchdbproxy_app,
    couchdbproxy_sup,
    couchdbproxy_web,
    couchdbproxy_deps,
	  couchdbproxy_http,
	  couchdbproxy_revproxy,
	  couchdbproxy_nodes,
	  couchdbproxy_routes,
	  couchdbproxy_util
	
  ]},
  {registered, [couchdbproxy_sup]},
  {mod, {couchdbproxy_app, []}},
  {applications, [kernel, stdlib, crypto, couchbeam]}]}.
