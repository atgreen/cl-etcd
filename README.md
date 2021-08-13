# cl-etcd

[![Build Status](https://github.com/atgreen/cl-etcd/actions/workflows/test.yaml/badge.svg)](https://github.com/atgreen/cl-etcd/actions)

This is a WIP experiment.

The purpose of `cl-etcd` is to simplify the writing of distributed
applications in Common Lisp.  [etcd](https://etcd.io/) is a strongly
consistent, distributed key-value store.  At it's core it is an
implementation of the [raft consensus
algorithm](https://en.wikipedia.org/wiki/Raft_(algorithm)).  GC pauses
in my lisp implementation of choice make it a poor choice for a
lisp-based raft implementation, and so we run `etcd` as an
asynchronous child process under lisp.

The `cl-etcd` package includes basic `put`, `get` and `watch`
functions, as well as the convenience macro `with-etcd` to make it
easy to start-up and shut-down your "embedded" etcd node.  You can
upply optional `:on-leader` and `on-follower` lambdas that will get
called when the node becomes the leader or follower respectively.

Etcd is configured for auto-TLS communication between peers, meaning
that the inter-node traffic will be encrypted with self-signed
certificates.  No form of authentication or encryption is currently
performed between etcd and the client (the `cl-etcd` library code),
however etcd is configured to only allow connections from localhost.

Look in the `test` directory for examples of how to use this.  As a
work-in-progress, it's very likely that details will change over time.
Feedback welcome!

AG
