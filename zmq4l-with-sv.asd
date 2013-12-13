(defpackage :zmq4l.sv.asdf
  (:use #:cl #:asdf))
(in-package :zmq4l.sv.asdf)

(defsystem :zmq4l-with-sv
  :description "ZMQ4L including static-vectors options"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:static-vectors :zmq4l)
  :pathname "src"
  :serial t

  :components
  ((:file "static-vectors")))
