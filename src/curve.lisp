(in-package :zmq4)

(defun z85-encode (data)
  (unless (= 0 (mod (length data) 4))
    (error "Length of DATA must be a multiple of 4"))
  (c-with ((dest :char :count (1+ (truncate (* 1.25 (length data)))))
           (c-data :char :count (length data)))
    ;; FIXME: slow
    (loop for i from 0 below (length data)
          do (setf (c-data i) (aref data i)))
    (check-nil (zmq-z85-encode (dest &) (c-data &) (length data)))))

(defun z85-decode (string)
  (unless (= 0 (mod (length string) 5))
    (error "Length of STRING must be a multiple of 5"))
  (let ((data-len (truncate (* 0.8 (length string)))))
    (c-with ((c-data :char :count data-len))
      (inhibit-string-conversion
        (check-null (nth-value 1 (zmq-z85-decode (c-data &) string))))
      (let ((data (make-array data-len :element-type '(unsigned-byte 8))))
        ;; FIXME: slow
        (loop for i from 0 below data-len
              do (setf (aref data i) (c-data i)))
        data))))

(defun curve-keypair ()
  (c-with ((pub :char :count 41)
           (sec :char :count 41))
    (check-rc (zmq-curve-keypair (pub &) (sec &)))
    (values (pub string) (sec string))))

