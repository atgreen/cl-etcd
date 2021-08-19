;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-ETCD; Base: 10 -*-
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
   (get-put-uri :reader get-put-uri)
   (id :reader id :initform nil)
   (on-leader :reader on-leader :initarg :on-leader :initform nil)
   (on-follower :reader on-follower :initarg :on-follower :initform nil)
   (ready :reader ready :initform nil)
   (version :reader version :initform nil)
   (start-semaphore :reader start-semaphore :initform (bt:make-semaphore))
   (role :reader role)))

(defmacro with-etcd ((etcd config &key on-leader on-follower) &body body)
  "Create an etcd subprocess, ETCD.  CONFIG is a hashtable of etcd
config options: name, initial-advertise-peer-urls, listen-peer-urls,
listen-client-urls, advertise-client-urls, initial-cluster,
initial-cluster-state, initial-cluster-token.  Otherwise, CONFIG is
nil and we are creating a non-clustered etcd instance."
  `(let ((,etcd (make-instance 'etcd :config ,config :on-leader ,on-leader :on-follower ,on-follower)))
     (unwind-protect
          (progn
            ,@body)
       (delete-process (process ,etcd)))))

(defparameter +became-regex+
  (cl-ppcre:create-scanner ".*[^0-9a-f]([0-9a-f]+) became ([a-z]+) at term.*"))

(defparameter +etcd-member-id-regex+
  (cl-ppcre:create-scanner ".*local-member-id...([0-9a-f]+)[^0-9a-f].*"))

(defun monitor-etcd-output (etcd s)
  ;; This function is called to process output from the etcd
  ;; subprocess.  Only one thread should ever be calling this
  ;; function, so we don't need to protect access to internal state
  ;; (eg. READY).
  (with-slots (ready version start-semaphore id role on-leader on-follower) etcd
    (unless version
      (when (search "starting an etcd server" s)
        (let* ((json (json:decode-json-from-string s))
               (vstring (cdr (assoc :etcd-version json)))
               (vnums (split-sequence:split-sequence #\. vstring))
               (v (+ (parse-integer (car vnums)) (/ (parse-integer (cadr vnums)) 10))))
          (when (< v 3.5)
            (error "etcd version 3.5 or higher is required, but we found version ~A" vstring))
          (setf version vstring))))
    (unless ready
      (when (search "ready to serve client requests" s)
        (setf ready t)
        (bt:signal-semaphore start-semaphore)))
    (unless id
      (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends +etcd-member-id-regex+ s)
        (setf id (subseq s (aref reg-starts 0) (aref reg-ends 0)))))
    (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends +became-regex+ s)
      (setf role (subseq s (aref reg-starts 1) (aref reg-ends 1)))
      (if (string= role "leader")
          (when on-leader (bt:make-thread (lambda () (funcall on-leader etcd))))
          (if (string= role "follower")
              (when on-follower (bt:make-thread (lambda () (funcall on-follower etcd)))))))))

(defmethod initialize-instance :after ((etcd etcd) &key)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program "which etcd" :ignore-error-status t)
    (when (not (= 0 exit-code))
      (error "etcd command is required but missing")))
  (with-slots (config start-semaphore process get-put-uri) etcd
    (flet ((get-config-value (key)
             (or (gethash key config)
                 (error "etcd config missing value for '~A'" key))))
      (setf get-put-uri (format nil "~A/v2/keys/"
                                (if config
                                    (get-config-value "listen-client-urls")
                                    "http://127.0.0.1:2379")))
      (let ((cmd (if config
                     `("etcd"
                       "--name" ,(get-config-value "name")
                       "--initial-advertise-peer-urls" ,(get-config-value "initial-advertise-peer-urls")
                       "--listen-peer-urls" ,(get-config-value "listen-peer-urls")
                       "--listen-client-urls" ,(get-config-value "listen-client-urls")
                       "--advertise-client-urls" ,(get-config-value "advertise-client-urls")
                       "--initial-cluster" ,(get-config-value "initial-cluster")
                       "--initial-cluster-state" "new"
                       "--initial-cluster-token" "cl-etcd-cluster"
                       "--peer-auto-tls"
                       "--host-whitelist" "127.0.0.1")
                     `("etcd" "--host-whitelist" "127.0.0.1"))))
        (setf (uiop:getenv "ETCD_ENABLE_V2") "true")
        (setf process (run-process cmd :name "etcd" :output-callback
                                   (lambda (s)
                                     (monitor-etcd-output etcd s))))
        (bt:wait-on-semaphore start-semaphore)))))

(defun (setf get-etcd) (value key etcd)
  "PUT the KEY/VALUE pair into ETCD."
  (with-slots (get-put-uri) etcd
    (multiple-value-bind (answer error-code)
        (drakma:http-request (concatenate 'string get-put-uri key)
                             :method :put
                             :content (format nil "value=~A" value))
      (when (and (not (= error-code 200)) (not (= error-code 201)))
        (error "can't store in etcd[~A]: ~A" error-code (flexi-streams:octets-to-string answer)))
      key)))

(defun get-etcd (key etcd)
  "GET the value of KEY from ETCD.  Returns NIL if KEY not found.
Throws an error on unexpected errors."
  (block get
    (let* ((code 0)
           (json (json:decode-json-from-string
                  (flexi-streams:octets-to-string
                   (with-slots (get-put-uri) etcd
                     (multiple-value-bind (answer error-code)
                         (drakma:http-request (concatenate 'string get-put-uri key)
                                              :method :get)
                       (when (= error-code 404)
                         (return-from get nil))
                       (setf code error-code)
                       answer))))))
      (when (not (= code 200))
        (error (cdr (assoc :message json))))
      (cdr (assoc :value (cdr (assoc :node json)))))))

(defun watch (etcd key)
  "Like GET, but waits until value changes."
  (let ((json (json:decode-json-from-string
               (flexi-streams:octets-to-string
                (with-slots (get-put-uri) etcd
                  (drakma:http-request (concatenate 'string get-put-uri key "?wait=true")
                                       :method :get))))))
    (when (assoc :error-code json)
      (error (cdr (assoc :message json))))
    (cdr (assoc :value (cdr (assoc :node json))))))
