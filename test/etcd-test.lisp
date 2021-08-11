(defpackage #:etcd-test
  (:use #:cl)
  (:shadow #:package)
  (:export #:start))

(in-package :etcd-test)

(defmethod cl-etcd:become-leader ((etcd cl-etcd:etcd))
  (cl-etcd:put etcd "hello" "world")
  (format t "**** I AM THE LEADER ***********"))

(defmethod cl-etcd:become-follower ((etcd cl-etcd:etcd))
  (format t "**** I AM A FOLLOWER ***********"))

(defun start ()
  (let ((config (cl-toml:parse
                 (alexandria:read-file-into-string "/etc/etcd-test/config.ini"
				                   :external-format :latin-1)))
        (etcd nil))
    (with-etcd (etcd config)
      (sleep 10)
      (print (cl-etcd:get etcd "hello"))
      (sleep 240))))
