(in-package :zmq4)

(defstruct (zmq-socket (:include autowrap:wrapper)
                       (:constructor %make-zmq-socket)))
(autowrap:define-foreign-alias 'zmq-socket :pointer)

(autowrap:define-enum-from-constants (zmq-connection-type)
  +zmq-req+
  +zmq-rep+
  +zmq-dealer+
  +zmq-router+
  +zmq-pub+
  +zmq-sub+
  +zmq-xpub+
  +zmq-xsub+
  +zmq-push+
  +zmq-pull+
  +zmq-pair+
  +zmq-stream+)

(defun socket (ctx type)
  (autocollect (ptr)
      (check-null
          (autowrap:wrap-pointer
           (zmq-socket ctx (enum-value 'zmq-connection-type type))
           'zmq-socket))
    (check-rc (zmq-close ptr))))

(defun close-socket (socket)
  (tg:cancel-finalization socket)
  (unwind-protect
       (check-rc (zmq-close socket))
    (invalidate socket))
  (values))

(defun bind (socket endpoint)
  (check-rc (zmq-bind socket endpoint)))

(defun unbind (socket endpoint)
  (check-rc (zmq-unbind socket endpoint)))

(defun connect (socket endpoint)
  (check-rc (zmq-connect socket endpoint)))

(defun disconnect (socket endpoint)
  (check-rc (zmq-disconnect socket endpoint)))

(autowrap:define-enum-from-constants (zmq-sockopt)
  +zmq-sndhwm+
  +zmq-rcvhwm+
  +zmq-affinity+
  +zmq-subscribe+
  +zmq-unsubscribe+
  +zmq-identity+
  +zmq-rate+
  +zmq-recovery-ivl+
  +zmq-sndbuf+
  +zmq-rcvbuf+
  +zmq-linger+
  +zmq-reconnect-ivl+
  +zmq-reconnect-ivl-max+
  +zmq-backlog+
  +zmq-maxmsgsize+
  +zmq-multicast-hops+
  +zmq-rcvtimeo+
  +zmq-sndtimeo+
  +zmq-ipv6+
  +zmq-ipv4only+
  +zmq-immediate+
  +zmq-router-mandatory+
  +zmq-router-raw+
  +zmq-probe-router+
  +zmq-xpub-verbose+
  +zmq-req-correlate+
  +zmq-req-relaxed+
  +zmq-tcp-keepalive+
  +zmq-tcp-keepalive-idle+
  +zmq-tcp-keepalive-cnt+
  +zmq-tcp-keepalive-intvl+
  +zmq-tcp-accept-filter+
  +zmq-plain-server+
  +zmq-plain-username+
  +zmq-plain-password+
  +zmq-curve-server+
  +zmq-curve-publickey+
  +zmq-curve-secretkey+
  +zmq-curve-serverkey+
  +zmq-zap-domain+
  +zmq-conflate+)

(defun %setsockopt (socket option ptr len)
  (check-rc (zmq-setsockopt socket (enum-value 'zmq-sockopt option)
                            ptr len)))

(defun setsockopt-string (socket option value)
  ;; value is declared as void* so autowrap doesn't infer :string
  (cffi:with-foreign-string ((string len) value)
    (%setsockopt socket option string len)))

(defun setsockopt-int (socket option value)
  (c-with ((i :int))
    (setf i value)
    (%setsockopt socket option (i &) (sizeof :int))))

(defun setsockopt-data (socket option value)
  (c-with ((data :char :count (length value)))
    ;; FIXME: slow
    (slow-copy-to-c value (data &))
    (%setsockopt socket option (data &) (length value))))

(defun setsockopt-i64 (socket option value)
  (c-with ((i64 int64-t))
    (setf i64 value)
    (%setsockopt socket option (i64 &) (sizeof 'int64-t))))

(defun setsockopt-u64 (socket option value)
  (c-with ((u64 uint64-t))
    (setf u64 value)
    (%setsockopt socket option (u64 &) (sizeof 'int64-t))))

(defun setsockopt (socket option value)
  (case option
    ((:affinity)
     (setsockopt-u64 socket option value))
    ((:maxmsgsize)
     (setsockopt-i64 socket option value))
    ((:subscribe :unsubscribe :identity :tcp-accept-filter)
     (setsockopt-data socket option value))
    ((:plain-username :plain-password :zap-domain)
     (setsockopt-string socket option value))
    ((:curve-publickey :curve-secretkey :curve-serverkey)
     (etypecase value
       (string (setsockopt-string socket option value))
       (array (setsockopt-data socket option value))))
    (t (setsockopt-int socket option value))))

(autowrap:define-enum-from-constants (zmq-sockopt-get)
  +zmq-type+
  +zmq-rcvmore+
  +zmq-sndhwm+
  +zmq-rcvhwm+
  +zmq-affinity+
  +zmq-identity+
  +zmq-rate+
  +zmq-recovery-ivl+
  +zmq-sndbuf+
  +zmq-rcvbuf+
  +zmq-linger+
  +zmq-reconnect-ivl+
  +zmq-reconnect-ivl-max+
  +zmq-backlog+
  +zmq-maxmsgsize+
  +zmq-multicast-hops+
  +zmq-rcvtimeo+
  +zmq-sndtimeo+
  +zmq-ipv6+
  +zmq-ipv4only+
  +zmq-immediate+
  +zmq-fd+
  +zmq-events+
  +zmq-last-endpoint+
  +zmq-tcp-keepalive+
  +zmq-tcp-keepalive-idle+
  +zmq-tcp-keepalive-cnt+
  +zmq-tcp-keepalive-intvl+
  +zmq-mechanism+
  +zmq-plain-server+
  +zmq-plain-username+
  +zmq-plain-password+
  +zmq-curve-publickey+
  +zmq-curve-secretkey+
  +zmq-curve-serverkey+
  +zmq-zap-domain+)

(defun %getsockopt (socket option ptr)
  (c-with ((len size-t))
    (check-rc (zmq-getsockopt socket (enum-value 'zmq-sockopt option)
                              ptr (len &)))
    len))

(defun getsockopt-string (socket option)
  (c-with ((pointer :pointer))
    (%getsockopt socket option (pointer &))
    (cffi:foreign-string-to-lisp pointer)))

(defun getsockopt-int (socket option)
  (c-with ((i :int))
    (%getsockopt socket option (i &))
    i))

(defun getsockopt-data (socket option)
  (c-with ((pointer :pointer))
    (let ((len (%getsockopt socket option (pointer &))))
      (c-with ((data :char :ptr pointer))
        (let ((value (make-array len :element-type '(unsigned-byte 8))))
          ;; FIXME: slow
          (slow-copy-to-lisp (data &) value))))))

(defun getsockopt-i64 (socket option)
  (c-with ((i64 int64-t))
    (%getsockopt socket option (i64 &))
    i64))

(defun getsockopt-u64 (socket option)
  (c-with ((u64 uint64-t))
    (%getsockopt socket option (u64 &))
    u64))

(defun getsockopt (socket option)
  (case option
    ((:identity :last-endpoint :plain-username :plain-password :zap-domain)
     (getsockopt-string socket option))
    ((:curve-publickey :curve-secretkey :curve-serverkey)
     (getsockopt-data socket option))
    ((:maxmsgsize)
     (getsockopt-i64 socket option))
    ((:affinity)
     (getsockopt-u64 socket option))
    (t (getsockopt-int socket option))))

(defun send (socket data &optional flags)
  (c-with ((c-data :char :count (length data)))
    ;; This is slow; use the STATIC-VECTORS stuff if you want fast
    (slow-copy-to-c data (c-data &))
    (check-rc (zmq-send socket (c-data &) (length data)
                        (mask-apply 'zmq-sendflags flags)))))

(defun recv (socket data &optional flags)
  "zmq_recv (length DATA) into DATA, which should be an array
of (unsigned-byte 8)"
  (c-with ((c-data :char :count (length data)))
    (check-rc
     (zmq-recv socket (c-data &) (length data)
               (mask-apply 'zmq-recvflags flags)))
    ;; This is slow; use the STATIC-VECTORS stuff if you want fast
    (slow-copy-to-lisp (c-data &) data)
    data))

(defstruct (pollitem (:constructor %make-pollitem))
  (socks nil)
  (zmq-pollitem nil)
  (nitems 0))

(autowrap:define-bitmask-from-constants (zmq-pollevent)
  +zmq-pollin+
  +zmq-pollout+
  +zmq-pollerr+)

(defun make-pollitem (&rest sockets-or-fds)
  "Specify sockets or FDs and the desired events,
e.g., (SOCK-OR-FD :in :out)"
  (let* ((len (length sockets-or-fds))
         (socks (make-array len)))
    (c-let ((pollitem zmq-pollitem-t :count len))
      (autocollect (ptr) pollitem
        (autowrap:free ptr))
      (loop for (s . mask) in sockets-or-fds
            for i from 0 below len
            do (etypecase s
                 (zmq-socket (setf (pollitem i :socket)
                                   (ptr s)))
                 (integer (setf (pollitem i :fd) s)))
               (setf (pollitem i :events)
                     (mask-apply 'zmq-pollevent mask))
               (setf (aref socks i) s))
      (%make-pollitem :socks socks
                      :zmq-pollitem pollitem
                      :nitems len))))

(defun poll (pollitem-or-list &optional (timeout 0))
  "Poll `POLLITEM-OR-LIST` with optional timeout `TIMEOUT`."
  (let ((pollitem (if (listp pollitem-or-list)
                      (apply #'make-pollitem pollitem-or-list)
                      pollitem-or-list)))
    (check-rc (zmq-poll (pollitem-zmq-pollitem pollitem)
                        (pollitem-nitems pollitem)
                        timeout))
    (c-with ((cpi zmq-pollitem-t :from (pollitem-zmq-pollitem pollitem)))
      (let (socks)
        (loop for i from 0 below (pollitem-nitems pollitem)
              do (when (/= 0 (cpi i :revents))
                   (push (list* (aref (pollitem-socks pollitem) i)
                                (mask-keywords 'zmq-pollevent
                                               (cpi i :revents)))
                         socks))
              finally (return (nreverse socks)))))))

(defmacro do-poll ((sock-var event-var &optional (timeout -1))
                   socks-or-fds end-test-form &body body)
  "Looping `zmq_poll()` on `SOCKS-OR-FDS` in the same format passed to
`MAKE-POLLITEM`.  Each socket will be assigned to `SOCK-VAR` and the
event list to `EVENT-VAR` and iterated for each `zmq_poll()`.  Loop
will continue until `END-TEST-FORM` returns `t`, or `zmq_poll()`
returns with no events.

Note that `TIMEOUT` defaults to -1, which means wait forever."
  (once-only (timeout)
    (with-gensyms (pollitem i cpi none-found)
      `(let ((,pollitem (make-pollitem
                         ,@(mapcar (lambda (x) `(list ,@x))
                                   socks-or-fds)))
             (,none-found))
         (c-with ((,cpi zmq-pollitem-t :from (pollitem-zmq-pollitem ,pollitem)))
           (loop until (or ,none-found ,end-test-form)
                 do (check-rc (zmq-poll (pollitem-zmq-pollitem ,pollitem)
                                        (pollitem-nitems ,pollitem)
                                        ,timeout))
                    (setf ,none-found t)
                    (loop for ,i from 0 below (pollitem-nitems ,pollitem)
                          do (when (/= 0 (,cpi ,i :revents))
                               (setf ,none-found nil)
                               (let ((,sock-var (aref (pollitem-socks ,pollitem) ,i))
                                     (,event-var (mask-keywords 'zmq-pollevent
                                                                (,cpi ,i :revents))))
                                 ,@body)))))))))

(defmacro do-pollitem ((sock-var event-var &optional (timeout -1))
                       pollitem end-test-form &body body)
  "Much like `DO-POLL`, except with a pollitem created by
`MAKE-POLLITEM`.  Perhaps more useful for real-world cases,
this *repeatedly evaluates* `POLLITEM`, which you may `SETF`
to a new pollitem as necessary (e.g., to remove or add new
sockets).

Otherwise, this is the same as `DO-POLL`."
  (once-only (timeout)
    (with-gensyms (i cpi none-found)
      `(let (,none-found)
         (c-with ((,cpi zmq-pollitem-t :from (pollitem-zmq-pollitem ,pollitem)))
           (loop until (or ,none-found ,end-test-form)
                 do (check-rc (zmq-poll (pollitem-zmq-pollitem ,pollitem)
                                        (pollitem-nitems ,pollitem)
                                        ,timeout))
                    (setf ,none-found t)
                    (loop for ,i from 0 below (pollitem-nitems ,pollitem)
                          do (when (/= 0 (,cpi ,i :revents))
                               (setf ,none-found nil)
                               (let ((,sock-var (aref (pollitem-socks ,pollitem) ,i))
                                     (,event-var (mask-keywords 'zmq-pollevent
                                                                (,cpi ,i :revents))))
                                 ,@body)))))))))
