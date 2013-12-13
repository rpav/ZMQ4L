# ZMQ4L

This is "yet another wrapper" for ZMQ.  This utilizes
[autowrap](https://github.com/rpav/cl-autowrap) and wraps version
4.0.1 as of writing.

This is a nearly-complete wrap, excepting the following:

* Functions documented as deprecated (`zmq_init()`, `zmq_recvmsg()`, etc)
* `zmq_send_const()` which is not particularly useful in the context of CL

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

;; May return something like:
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

### STATIC-VECTORS

If desired, ZMQ4L supports
[static-vectors](https://github.com/sionescu/static-vectors/).  To
use, you may load the system `:zmq4l-with-sv`, which will load ZMQ4L,
static-vectors, and the appropriate additional definitions.

The following functions are added:

* `send-sv SOCKET STATIC-VECTOR &optional FLAGS`: Like `send`, but the
  contents are pulled directly from `STATIC-VECTOR`.  You must still
  manage the static vector.  This is likely considerably faster than
  `send`, because the contents are not copied byte-by-byte from a lisp
  array to foreign.
* `recv-sv SOCKET STATIC-VECTOR &optional FLAGS`: Like `recv`, but the
  contents are written directly to `STATIC-VECTOR`.  You must still
  manage the static vector.  This is likely considerably faster than
  `send`, because the contents are not copied byte-by-byte from a
  foreign array to lisp.
* `msg-init-sv STATIC-VECTOR`: Like `msg-init` with a standard array,
  except this uses `memcpy()` rather than a slower byte-by-byte copy
  from lisp to a foreign array.  You must still manage the static
  vector.
* `msg-init-data STATIC-VECTOR &optional FREE-FUN`: Create a message,
  using the data in `STATIC-VECTOR` directly.  By default, `FREE-FUN`
  is `free-static-vector`, which will free the static vector.  This
  means that after `MSG-CLOSE` or `MSG-SEND`, the static-vector *must
  not be accessed further*, because it may be freed immediately.
  Caveats apply, see below.

These functions will not be defined if `:zmq4l-with-sv` is not loaded,
but the symbols will be exported.

**Caveats:**  The use of `msg-init-data` *requires* lisp functions be
callable in *arbitrary foreign threads*, including those your lisp may
not know about.  By default, SBCL *does not support this* except on
Windows.  CCL does support this.  I am not sure of the status of other
lisps; *use at your own discretion*.

For SBCL, you may build with the options `--with-sb-safepoint
--with-sb-thruption --with-sb-wtimer` to enable foreign thread
callbacks.  This is reportedly safe on Linux x86_64.

`send-sv`, `recv-sv`, and `msg-init-sv` are safe regardless.
