# ZMQ4L

This is "yet another wrapper" for ZMQ.  This utilizes
[autowrap](https://github.com/rpav/cl-autowrap) and wraps version
4.0.1 as of writing.

This is a nearly-complete wrap, excepting the following:

* `STATIC-VECTORS` support (coming soon) for maximum efficiency,
  and thus `zmq_send_const()`
* Functions documented as deprecated (`zmq_init()`, `zmq_recvmsg()`,
  etc).

As of writing you will need the latest version of autowrap from the
repository.

## API

### Overview

The API coverage is similar to the ZMQ C API such that the C
documentation should translate fairly directly.  Functions are
exported from the `ZMQ4` package and do not have a `ZMQ-` prefix.
For instance, `zmq_ctx_new()` is now `zmq4:ctx-new`, etc.

Exceptions:

* `zmq_close()` -> `zmq4:close-socket`: To avoid shadowing `CL:CLOSE`.
* `zmq4:msg-init` creates a message, in addition to initializing

As per normal autowrap, shortest-suffix keywords may be used instead
of numeric constants for enums and masks.  For instance:

```lisp
;; Masks use a list of keywords:
(zmq4:send SOCK DATA '(:dontwait))
(zmq4:send SOCK DATA '(:dontwait :sndmore))

;; Enums use a lone keyword:
(zmq4:getsockopt SOCK :type)
```

### POLL

Poll is perhaps of particular note since it's a bit more complex than
the rest of the API.

```lisp
(let ((pollitem (zmq4:make-pollitem (list sock1 :in)
                                    (list sock2 :out ...))))
  (poll pollitem TIMEOUT))

;; May return somethng like:
;; ((#<SOCKET ...> :IN) ...)
```

Sockets in this case can be `ZMQ-SOCKET` or integer file descriptors.
This may be a bit wordy, so provided are the following macros:

```lisp
(zmq4:do-poll (sock events &optional TIMEOUT)
  ((sock1 :in)
   (sock2 :in :out :err))
   END-TEST-FORM

   body...)
```

This will loop and interate over each socket with events.  It finishes
when `END-TEST-FORM` evaluates to a non-NIL value, or `TIMEOUT` is
reached without any events.  In this case, `sock` and `events` will be
assigned to, for instance, the value of `sock1` and `(:in)`,
respectively.

Note that `END-TEST-FORM` is only evaluated between calls to
`zmq_poll()`, so more sockets may be iterated before the loop quits.

Alternatively, there is `DO-POLLITEM`, which provides a slightly more
robust loop:

```lisp
(zmq4:do-pollitem (sock events &optional TIMEOUT)
   POLLITEM-FORM
   END-TEST-FORM

   body...)
```

In this case, `POLLITEM-FORM` is a pollitem created by `make-pollitem`, and
is evaluated *every iteration* before `zmq_poll()`.  Thus, you may use
a variable and reassign it to a new pollitem, if for instance you wish
to remove or add sockets.  Otherwise, the semantics are the same.
