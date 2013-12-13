(in-package :zmq4)

(defun msg-init (&optional data-or-size)
  "Create a new message, optionally specifying an octet vector,
string, or a size to initialize.  Unlike zmq_msg_init(), this
actually allocates a message."
  (c-let ((msg zmq-msg-t))
    (autocollect (ptr) msg
      (zmq-msg-close ptr)
      (autowrap:free ptr))
    (etypecase data-or-size
      (integer
       (check-rc (zmq-msg-init-size msg data-or-size)))
      (array
       ;; For zmq_msg_init_data you need STATIC-VECTORS
       (check-rc (zmq-msg-init-size msg (length data-or-size)))
       (c-with ((c-data :char :ptr (zmq-msg-data msg)))
         (slow-copy-to-c data-or-size (c-data &))))
      (null
       (check-rc (zmq-msg-init msg))))
    msg))

(defun msg-size (msg)
  (zmq-msg-size msg))

(defun msg-data (msg)
  "Make a copy of the data in MSG as a lisp octet vector.  Copies
byte-by-byte."
  (let ((data (make-array (msg-size msg) :element-type '(unsigned-byte 8))))
    (c-with ((c-data :unsigned-char :ptr (zmq-msg-data msg)))
      (slow-copy-to-lisp (c-data &) data))
    data))

(defun msg-string (msg)
  "Make a copy of the data in MSG to a lisp string.  May be slow."
  (c-with ((c-data :char :ptr (zmq-msg-data msg)))
    (cffi:foreign-string-to-lisp (c-data &) :count (zmq-msg-size msg))))

(defun (setf msg-string) (v msg)
  "Set the data in `MSG` from the string value `V`."
  (unless (= (length v) (msg-size msg))
    (error "ZMQ MSG and string value to assign are not the same length"))
  (c-with ((c-data :char :ptr (zmq-msg-data msg)))
    (cffi:lisp-string-to-foreign v (c-data &) (length v)))
  v)

(defun (setf msg-data) (v msg)
  "Set the data in `MSG` from the octet vector `V`."
  (unless (= (length v) (msg-size msg))
    (error "ZMQ MSG and data to assign are not the same length"))
  (c-with ((c-data :char :ptr (zmq-msg-data msg)))
    (slow-copy-to-c v (c-data &)))
  v)

(defun msg-copy (msg &optional dest)
  "Copy the data in `MSG` to `DEST` or a new message object."
  (let ((new-msg (or dest (msg-init))))
    (check-rc (zmq-msg-copy new-msg msg))
    new-msg))

(defun msg-move (msg &optional dest)
  "Move the data in `MSG` to `DEST` or a new message object."
  (let ((new-msg (or dest (msg-init))))
    (check-rc (zmq-msg-move new-msg msg))))

(defun msg-close (msg)
  "Mark internal ZMQ data related to `MSG` as no longer required.
It is generally unnecessary to call this, since ZMQ4L will call
this automatically."
  (tg:cancel-finalization msg)
  (check-rc (zmq-msg-close msg))
  (autowrap:free msg)
  (invalidate msg)
  (values))

(defun msg-send (msg socket &optional flags)
  "Send `MSG` on socket, destroying `MSG`.  As per zmq_msg_send(),
if a copy is required, first use `MSG-COPY`."
  (check-rc (zmq-msg-send msg socket
                          (mask-apply 'zmq-sendflags flags)))
  (tg:cancel-finalization msg)
  (autowrap:free msg)
  (invalidate msg)
  (values))

(defun msg-recv (socket &optional flags)
  "Receive a message from `SOCKET`. `FLAGS` may be a list of valid
flags for zmq_msg_recv()."
  (let ((msg (msg-init)))
    (check-rc (zmq-msg-recv msg socket
                            (mask-apply 'zmq-recvflags flags)))
    msg))

(autowrap:define-enum-from-constants (zmq-getprop "ZMQ-")
  +zmq-more+)

(defun msg-get (msg prop)
  (check-rc (zmq-msg-get msg (enum-value 'zmq-getprop prop))))

(defun msg-set (msg prop value)
  (check-rc (zmq-msg-set msg prop value)))

(defun msg-more-p (msg)
  "Check whether there are further parts to message `MSG`."
  (= 1 (check-rc (zmq-msg-more msg))))


