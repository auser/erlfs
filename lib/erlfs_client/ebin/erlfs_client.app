{application, erlfs_client, [{mod, {erlfs_client, []}},
  	      {description, "Erlang distributed file storage system client."},
  	      {vsn, "0.1.0.0"},
	      {registered, [erlfs_client_svr]},
	      {applications, [kernel, stdlib, sasl]},
	      {modules, [erlfs_client, erlfs_client_sup,
	      		erlfs_client_svr, erlfs_client_lib]}]}.
