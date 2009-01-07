{application, erlfs_store, [{mod, {erlfs_store_app, []}},
	      {description, "Erlang distributed file storage system storage node application."},
	      {vsn, "0.2.3.0"},
	      {registered, [erlfs_store_svr]},
	      {applications, [kernel, stdlib, sasl, crypto]},
	      {modules, [erlfs_store_app, erlfs_store, erlfs_store_sup,
	      		erlfs_store_svr, erlfs_store_lib, erlfs_store_test]}]}.
