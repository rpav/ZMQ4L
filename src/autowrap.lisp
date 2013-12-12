(in-package :zmq4.ffi)

(autowrap:c-include '(zmq4l autospec "zmq.h")
  :spec-path '(zmq4l autospec)
  :exclude-arch ("i686-pc-win32")
  :exclude-sources ("stdio.h" "libio.h" "string.h" "stdlib.h" "types.h"
                    "time.h" "locale.h" "sys/select.h")
  :exclude-definitions ("^_" "^va_list$")
  :no-accessors t)
