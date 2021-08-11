(defpackage #:etcd-test
  (:use #:cl)
  (:shadow #:package)
  (:export #:start))

(in-package :etcd-test)

(defmethod cl-etcd:become-leader ((etcd cl-etcd:etcd))
  (format t "**** I AM THE LEADER ***********~%")
  (cl-etcd:put etcd "hello" "world"))

(defmethod cl-etcd:become-follower ((etcd cl-etcd:etcd))
  (format t "**** I AM A FOLLOWER ***********~%"))

(defun start ()
  (let ((config (cl-toml:parse
                 (alexandria:read-file-into-string "/etc/etcd-test/config.ini"
				                   :external-format :latin-1)))
        (etcd nil))
    (cl-etcd:with-etcd (etcd (gethash "etcd" config))
      ;; Future versions shouldn't need this sleep.  with-etcd should
      ;; wait until etcd is ready to accept client traffic.
      (sleep 15)
      (format t "~A: hello: ~A~%" (cl-etcd:id etcd) (cl-etcd:get etcd "hello")))))
