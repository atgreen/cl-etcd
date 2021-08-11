# cl-etcd

This is a WIP experiment.

The idea behind `cl-etcd` is to simplify writing distributed apps in
Common Lisp.  `etcd` is a strongly consistent, distributed key-value
store.  At it's core it is an implementation of the raft algorithm.
GC pauses in my lisp implementation of choice make it a poor choice
for a lisp-based raft implementation, and so we run `etcd` as an
asynchronous child process under lisp.

The `etcd` package includes basic `put` and `get` functions, as well
as to convenience macro `with-etcd` to make it simple to start-up your
`etcd` node.  The `etcd` package will call used defined
`become-leader` and `become-follower` methods when the node becomes
the leader or follower respectively.

AG
