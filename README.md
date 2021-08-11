# cl-etcd

This is a WIP experiment.

The purpose of `cl-etcd` is to simplify writing distributed apps in
Common Lisp.  [etcd](https://etcd.io/) is a strongly consistent,
distributed key-value store.  At it's core it is an implementation of
the [raft consensus
algorithm](https://en.wikipedia.org/wiki/Raft_(algorithm)).  GC pauses
in my lisp implementation of choice make it a poor choice for a
lisp-based raft implementation, and so we run `etcd` as an
asynchronous child process under lisp.

The `cl-etcd` package includes basic `put` and `get` functions, as
well as the convenience macro `with-etcd` to make it easy to start-up
and shut-down your "embedded" etcd node.  The `cl-etcd` package will
call user defined `become-leader` and `become-follower` methods when
the node becomes the leader or follower respectively.

Look in the `test` directory for examples of how to use this.  As a
work-in-progress, it's very likely that details will change over time.
Feedback welcome!

AG
