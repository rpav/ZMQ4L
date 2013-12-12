(defpackage :zmq4l.asdf
  (:use #:cl #:asdf))

(in-package :zmq4l.asdf)

(defsystem :zmq4l
  :description "ZMQ4 wrapper for CL"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:alexandria :cl-autowrap :cl-plus-c :trivial-garbage)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "autowrap")
   (:file "zmq4")
   (:file "ctx")
   (:file "msg")
   (:file "socket")
   (:file "curve")
   (:module #:autospec
    :pathname "autospec"
    :components
    ((:static-file "zmq.h")))))
