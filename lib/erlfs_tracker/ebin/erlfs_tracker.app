{application, erlfs_tracker, [{mod, {erlfs_tracker, []}},
	      {description, "Erlang distributed file storage system tracker node application."},
	      {vsn, "0.1.0.0"},
	      {registered, [erlfs_tracker_svr]},
	      {applications, [kernel, stdlib, sasl, crypto]},
	      {modules, [erlfs_tracker, erlfs_tracker_sup,
	      		erlfs_tracker_svr, erlfs_tracker_lib]}]}.
