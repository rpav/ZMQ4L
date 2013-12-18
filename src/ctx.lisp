(in-package :zmq4)

(defstruct (zmq-ctx (:include autowrap:wrapper)
                    (:constructor %make-zmq-ctx)))
(autowrap:define-foreign-alias 'zmq-ctx :pointer)

(defun ctx-new ()
  (autocollect (ptr)
      (check-null
          (autowrap:wrap-pointer (zmq-ctx-new) 'zmq-ctx))
    (zmq-ctx-destroy ptr)))

(defun ctx-term (ctx)
  (tg:cancel-finalization ctx)
  (unwind-protect
       (check-rc (zmq-ctx-term ctx))
    (invalidate ctx))
  (values))

(defun ctx-shutdown (ctx)
  (check-rc (zmq-ctx-shutdown ctx)))

(autowrap:define-enum-from-constants (zmq-ctx-options)
  +zmq-io-threads+
  +zmq-max-sockets+
  +zmq-ipv6+)

(defun ctx-set (ctx option value)
  (check-rc
   (zmq-ctx-set ctx (enum-value 'zmq-ctx-options option) value)))

(defun ctx-get (ctx option)
  (check-rc
   (zmq-ctx-get ctx (enum-value 'zmq-ctx-options option))))

(defmacro with-ctx ((ctx &rest option-pairs) &body body)
  `(let ((,ctx (ctx-new)))
    (unwind-protect
         (progn
           ,@(loop for opt in option-pairs by #'cddr
                   for val in (cdr option-pairs) by #'cddr
                   collect `(ctx-set ,ctx ,opt ,val))
           ,@body)
      (ctx-shutdown ,ctx))))

