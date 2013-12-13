(defpackage :zmq4.ffi
  (:use))

(defpackage :zmq4
  (:use #:cl #:alexandria #:autowrap.minimal #:plus-c #:zmq4.ffi)
  (:export

   ;; General
   #:errno #:strerror #:version

   ;; CTX
   #:ctx-new #:ctx-term #:ctx-shutdown #:ctx-set #:ctx-get
   #:with-ctx

   ;; Socket
   #:socket #:close-socket #:bind #:unbind #:connect #:disconnect

   #:zmq-sockopt
   #:setsockopt-string #:setsockopt-int #:setsockopt-data
   #:setsockopt-i64 #:setsockopt-u64 #:setsockopt

   #:zmq-sockopt-get
   #:getsockopt-string #:getsockopt-int #:getsockopt-data
   #:getsockopt-i64 #:getsockopt-u64 #:getsockopt

   #:send #:recv #:make-pollitem #:poll #:do-poll #:do-pollitem
   #:proxy #:socket-monitor

   ;; Curve
   #:z85-encode #:z85-decode #:curve-keypair

   ;; Messages
   #:msg-init #:msg-size #:msg-data #:msg-string #:msg-copy
   #:msg-move #:msg-close #:msg-send #:msg-recv
   ))
