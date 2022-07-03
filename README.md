# cl-etcd

[![Build Status](https://github.com/atgreen/cl-etcd/actions/workflows/test.yaml/badge.svg)](https://github.com/atgreen/cl-etcd/actions)

The purpose of `cl-etcd` is to simplify the writing of distributed
applications in Common Lisp.  [etcd](https://etcd.io/) is a strongly
consistent, distributed key-value store.  At it's core is an
implementation of the [raft consensus
algorithm](https://en.wikipedia.org/wiki/Raft_(algorithm)).  GC pauses
in my lisp implementation of choice make it a poor choice for a
lisp-based raft implementation, and so we run `etcd` as an
asynchronous child process under lisp.

The `cl-etcd` package includes basic `get-etcd` and `watch` functions,
as well as the convenience macro `with-etcd` to make it easy to
start-up and shut-down your "embedded" etcd node.  `get-etcd` is
`setf`-able so you can easily modify the key-value store the Lisp way.

If you want to be notified about leader state changes on your node,
supply the optional `:on-leader` and `:on-follower` lambdas, and they
will be called when the node becomes a cluster leader or follower
respectively.

Etcd is configured for auto-TLS communication between peers, meaning
that the inter-node traffic will be encrypted with self-signed
certificates.  No form of authentication or encryption is currently
performed between etcd and the client (the `cl-etcd` library code),
however etcd is configured to only allow connections from localhost.

Here's a trivial example of a single node:

    (with-etcd (etcd nil)
      (setf (get-etcd "hello" etcd) "world")
      (get-etcd "hello" etcd))

To be notified on state changes to leader or follower, do this:

    (defun become-leader (etcd)
      (print "I'm the leader!"))

    (defun become-follower (etcd)
      (print "I'm a follower!"))

    (with-etcd (etcd nil :on-leader #'become-leader :on-follower #'become-follower)
      (setf (get-etcd "hello" etcd) "world")
      (get-etcd "hello" etcd))

A single instance is pretty useless, so let's make a 3-node cluster!
The second argument to `with-etcd` is a hashtable of etcd arguments.
The easiest way to populate this is through TOML config files.  Let's
make three config files like so...

`config1.ini`:

    [etcd]
    name = "infra0"
    initial-advertise-peer-urls = "http://127.0.0.1:2380"
    listen-peer-urls = "http://127.0.0.1:2380"
    listen-client-urls = "http://127.0.0.1:2379"
    advertise-client-urls = "http://127.0.0.1:2379"
    initial-cluster = "infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580"
    host-whitelist = "127.0.0.1"

`config2.ini`:

    [etcd]
    name = "infra1"
    initial-advertise-peer-urls = "http://127.0.0.1:2480"
    listen-peer-urls = "http://127.0.0.1:2480"
    listen-client-urls = "http://127.0.0.1:2479"
    advertise-client-urls = "http://127.0.0.1:2479"
    initial-cluster = "infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580"
    host-whitelist = "127.0.0.1"

`config2.ini`:

    [etcd]
    name = "infra2"
    initial-advertise-peer-urls = "http://127.0.0.1:2580"
    listen-peer-urls = "http://127.0.0.1:2580"
    listen-client-urls = "http://127.0.0.1:2579"
    advertise-client-urls = "http://127.0.0.1:2579"
    initial-cluster = "infra0=http://127.0.0.1:2380,infra1=http://127.0.0.1:2480,infra2=http://127.0.0.1:2580"
    host-whitelist = "127.0.0.1"

Now, in each process, load the appropriate config file:

    (defun become-leader (etcd)
      (setf (get-etcd "hello" etcd) "world"))

    (let ((config (cdr (assoc :etcd (cl-toml:parse-file "config1.ini")))))
      (with-etcd (etcd config :on-leader #'become-leader :on-follower #'become-follower)
        (sleep 3)
        (get-etcd "hello" etcd)))

As this is a work-in-progress, details may change, and feedback is always welcome.

AG
