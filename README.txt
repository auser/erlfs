ErlFS, a distributed storage system which uses distributed Erlang at it's core. The idea is that it splits files into chunks and copies them, redundantly over multiple disks on multiple machines. This system should scale nearly linearly-- that is, the more machines/disks you add, the more storage, throughput, response and reliability you get.

Each node keeps track of where the file chunks are located and will load balance automatically.

- Matt Williamson (http://dawsdesign.com/drupal/erlfs)
- Ari Lerner (http://blog.xnot.org)