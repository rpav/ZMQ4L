(in-package :zmq4)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (autowrap:define-foreign-function '(memcpy "memcpy") :pointer
    '((dest :pointer)
      (src :pointer)
      (n size-t))))

(defun send-sv (socket static-vector &optional flags)
  (check-rc (zmq-send socket
                      (static-vectors:static-vector-pointer static-vector)
                      (length static-vector)
                      (mask-apply 'zmq-sendflags flags))))

(defun recv-sv (socket static-vector &optional flags)
  (check-rc (zmq-recv socket
                      (static-vectors:static-vector-pointer static-vector)
                      (length static-vector)
                      (mask-apply 'zmq-recvflags flags))))

(defun msg-init-sv (static-vector)
  (c-let ((msg zmq-msg-t))
    (autocollect (ptr) msg
      (zmq-msg-close ptr)
      (autowrap:free ptr))
    (check-rc (zmq-msg-init-size msg (length static-vector)))
    (c-with ((c-data :char :ptr (zmq-msg-data msg)))
      (c-fun memcpy (c-data &) (ptr msg) (length static-vector)))
    msg))

(defvar *static-vector-to-free-fun*
  (make-hash-table))

(defcallback msg-free-static-vector-cb :void
    ((data :pointer)
     (hint :pointer))
  (declare (ignore hint))
  (let* ((id (cffi-sys:pointer-address data))
         (cb (gethash id *static-vector-to-free-fun*)))
    (if cb
        (progn
          (funcall (car cb) (cdr cb))
          (remhash id *static-vector-to-free-fun*))
        (warn "ZMQ4L:MSG-FREE callback called with ~A, but data not found!"
              data))))

#+(or (and sbcl sb-thread sb-safepoint sb-thruption sb-wtimer)
      (not sbcl))
(defun msg-init-data (static-vector
                      &optional (free-fun #'static-vectors:free-static-vector))
  "Create a new message from `STATIC-VECTOR`.  Memory will not be copied, thus,
the static-vector in question *must* be remain until `FREE-FUN` is
called.

By default, `FREE-FUN` calls `free-static-vector`, which means that
after sending the message, the static-vector *must no longer be used*,
as it may be freed at any time, including before `msg-send` returns.
If you wish to save the static vector, you must specify a custom
`FREE-FUN`, which will receive the static vector as an argument.

Additionally, this function *only works* if your Lisp supports lisp
callbacks *on arbitrary foreign threads*."
  (c-let ((msg zmq-msg-t))
    (autocollect (ptr) msg
      (zmq-msg-close ptr)
      (autowrap:free ptr))
    (check-rc (zmq-msg-init-data msg
                                 (static-vectors:static-vector-pointer static-vector)
                                 (length static-vector)
                                 (callback 'msg-free-static-vector-cb)
                                 nil))
    (let ((id (cffi-sys:pointer-address
               (static-vectors:static-vector-pointer static-vector))))
      (setf (gethash id *static-vector-to-free-fun*)
            (cons free-fun static-vector)))
    msg))
