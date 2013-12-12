(in-package :zmq4)

(defmacro check-null (form)
  (once-only (form)
    `(if (wrapper-null-p ,form)
         (error "NULL returned from ~A" ',form)
         ,form)))

(defmacro check-nil (form)
  (once-only (form)
    `(if (null ,form)
         (error "NIL returned from ~A" ',form)
         ,form)))

(defmacro check-rc (form)
  (once-only (form)
    `(if (< ,form 0)
         (let* ((errno (zmq-errno))
                (string (zmq-strerror errno)))
           (error "ZMQ error (~A): ~A" errno string))
         ,form)))

(cffi:define-foreign-library zeromq4
  (t (:default "libzmq")))

(cffi:use-foreign-library zeromq4)

(defun errno () (zmq-errno))
(defun strerror (errno) (values (zmq-strerror errno)))

(defun version ()
  (c-with ((major :int)
           (minor :int)
           (patch :int))
    (zmq-version (major &) (minor &) (patch &))
    (values major minor patch)))

(autowrap:define-bitmask-from-constants (zmq-sendflags)
  +zmq-dontwait+
  +zmq-sndmore+)

(autowrap:define-bitmask-from-constants (zmq-recvflags "^ZMQ-")
  +zmq-dontwait+)

(declaim (inline slow-copy-to-c slow-copy-to-lisp))
(defun slow-copy-to-c (lisp-data c-data)
  (c-with ((c-data :char :ptr c-data))
    (loop for i from 0 below (length lisp-data)
          do (setf (c-data i) (aref lisp-data i)))
    c-data))

(defun slow-copy-to-lisp (c-data lisp-data)
  (c-with ((c-data :unsigned-char :ptr c-data))
    (loop for i from 0 below (length lisp-data)
          do (setf (aref lisp-data i) (c-data i))))
  lisp-data)
