;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ETCD; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :cl-etcd)

(defclass process ()
  ((pointer :initarg :pointer :reader process-pointer)
   (name :initarg :name :reader process-name)
   (command :initarg :command :reader process-command)
   (buffer-stream :initarg :buffer-stream :reader process-buffer-stream)
   (read-thread :initarg :read-thread :writer set-process-read-thread :reader process-read-thread)
   (output-callback :initarg :output-callback :reader process-output-callback)
   (callback-type :initarg :output-callback-type :reader process-output-callback-type)))

(defun run-process (command &key name output-callback output-callback-type directory)
  (setf command (uiop:ensure-list command))
  (let ((buffer-stream (make-string-output-stream)))
    (let* ((pointer (async-process:create-process command :nonblock nil :directory directory))
           (process (make-instance 'process
                                   :pointer pointer
                                   :name name
                                   :command command
                                   :buffer-stream buffer-stream
                                   :output-callback output-callback
                                   :output-callback-type output-callback-type))
           (thread (bt:make-thread
                    (lambda ()
                      (loop
                        (unless (async-process:process-alive-p pointer)
                          (return))
                        (alexandria:when-let
                            (string (async-process:process-receive-output pointer))
                          (write-to-buffer process string))))
                    :name (format nil "run-process ~{~A~^ ~}" command))))
      (set-process-read-thread thread process)
      process)))

(defun get-process-output-string (process)
  (get-output-stream-string (process-buffer-stream process)))

(defun write-to-buffer (process string)
  (let ((buffer-stream (process-buffer-stream process))
        (output-callback (process-output-callback process))
        (output-callback-type (process-output-callback-type process)))
    (write-string string buffer-stream)
    (when output-callback
      (case output-callback-type
        (:process-input
         (funcall output-callback process string))
        (otherwise
         (funcall output-callback string))))))

(defun delete-process (process)
  (when (bt:thread-alive-p (process-read-thread process))
    (bt:destroy-thread (process-read-thread process)))
  (async-process:delete-process (process-pointer process)))

(defun process-send-input (process string)
  (async-process:process-send-input (process-pointer process) string))

(defclass etcd ()
  ((config :reader config :initarg :config)
   (process :reader process)
   (put-uri :reader put-uri)
   (range-uri :reader range-uri)
   (id :reader id :initform nil)
   (role :reader role)))

(defgeneric become-leader (etcd))
(defgeneric become-follower (etcd))

(defmacro with-etcd ((etcd config) &body body)
  `(let ((,etcd (make-instance 'etcd :config ,config)))
     (unwind-protect
          (progn
            ,@body)
       (delete-process (process ,etcd)))))

(defparameter +became-regex+
  (cl-ppcre:create-scanner ".*[^0-9a-f]([0-9a-f]+) became ([a-z]+) at term.*"))

(defparameter +etcd-member-id-regex+
  (cl-ppcre:create-scanner ".*local-member-id...([0-9a-f]+)[^0-9a-f].*"))

(defun monitor-etcd-output (etcd s)
  (with-slots (id role) etcd
    (unless id
      (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends +etcd-member-id-regex+ s)
        (setf id (subseq s (aref reg-starts 0) (aref reg-ends 0)))
        (log:debug "etcd node ID: ~A" id)))
    (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends +became-regex+ s)
      (setf role (subseq s (aref reg-starts 1) (aref reg-ends 1)))
      (log:debug "~A is a ~A" id role)
      (if (string= role "leader")
          (become-leader etcd)
          (if (string= role "follower")
              (become-follower etcd))))))

(defmethod initialize-instance :after ((etcd etcd) &key)
  (with-slots (config process put-uri range-uri) etcd
    (setf put-uri (format nil "~A/v3/kv/put" (gethash "listen-client-urls" config)))
    (setf range-uri (format nil "~A/v3/kv/range" (gethash "listen-client-urls" config)))
    (let ((cmd `("etcd"
                 "--name" ,(gethash "name" config)
                 "--initial-advertise-peer-urls" ,(gethash "initial-advertise-peer-urls" config)
                 "--listen-peer-urls" ,(gethash "listen-peer-urls" config)
                 "--listen-client-urls" ,(gethash "listen-client-urls" config)
                 "--advertise-client-urls" ,(gethash "advertise-client-urls" config)
                 "--initial-cluster" ,(gethash "initial-cluster" config)
                 "--initial-cluster-state" "new"
                 "--initial-cluster-token" "cl-etcd-cluster"
                 "--peer-auto-tls"
                 "--host-whitelist" "127.0.0.1")))
      (setf process (run-process cmd :name "etcd" :output-callback
                                 (lambda (s)
                                   (monitor-etcd-output etcd s)))))))

(defun put (etcd key value)
  (let ((json (json:encode-json-to-string
               `((:KEY . ,(cl-base64:string-to-base64-string key))
                 (:VALUE . ,(cl-base64:string-to-base64-string value))))))
    (with-slots (put-uri) etcd
      (drakma:http-request put-uri
                           :method :post
                           :content json))))

(defun get (etcd key)
  (let ((json (json:decode-json-from-string
               (flexi-streams:octets-to-string
                (with-slots (range-uri) etcd
                  (drakma:http-request range-uri
                                       :method :post
                                       :content (json:encode-json-to-string
                                                 `((:KEY . ,(cl-base64:string-to-base64-string key))))))))))
    (cl-base64:base64-string-to-string (cdr (assoc :value (car (cdr (assoc :kvs json))))))))
