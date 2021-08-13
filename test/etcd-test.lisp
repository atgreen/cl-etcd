;; Simple example.  Warning -- not much thought was put into
;; concurrency issues.

(defpackage #:etcd-test
  (:use #:cl)
  (:shadow #:package)
  (:export #:start))

(in-package :etcd-test)

(defvar *leader?* nil)

;; This method is called when I become leader.
(defun become-leader (etcd)
  (format t "**** I AM THE LEADER ***********~%")
  (setf *leader?* t)
  (setf (cl-etcd:get-etcd "hello" etcd) "world"))

;; This method is called when I become a follower.
(defun become-follower (etcd)
  (format t "**** I AM A FOLLOWER ***********~%")
  (setf *leader?* nil))

(defun start ()
  (let ((config (cl-toml:parse
                 (alexandria:read-file-into-string "/etc/etcd-test/config.ini"
				                   :external-format :latin-1)))
        (etcd nil))
    (cl-etcd:with-etcd (etcd (gethash "etcd" config)
                        :on-leader #'become-leader
                        :on-follower #'become-follower)
      ;; Future versions shouldn't need this sleep.  with-etcd should
      ;; wait until etcd is ready to accept client traffic.
      (sleep 15)
      (format t "~A: hello: ~A~%" (cl-etcd:id etcd) (cl-etcd:get etcd "hello"))
      (if *leader?*
          (progn
            (sleep 15)
            (setf (cl-etcd:get-etcd "hello" etcd) "again"))
          (format t "~A: hello: ~A~%" (cl-etcd:id etcd) (cl-etcd:watch etcd "hello")))
      (format t ">>> DONE - sleeping for 40 <<<")
      (sleep 40))))
