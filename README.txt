I've begun work on ErlFS, a distributed storage system which uses distributed Erlang at it's core. The idea is that it splits files into chunks and copies them, redundantly over multiple disks on multiple machines. This system should scale nearly linearly-- that is, the more machines/disks you add, the more storage, throughput, response and reliability you get.

Each node keeps track of where the file chunks are located and will load balance automatically. I hope to have an Erlang, HTTP, FTP and Bittorrent interface for the system, probably using YAWS/erlyweb.

Google code is hosting the project for me. You can view it at http://code.google.com/p/erlfs/.

I will update here when an alpha is ready.

Update: We just registered http://erlfs.com, so we will have a home for it.