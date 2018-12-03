(defparameter *data* '())
(defparameter *counts* (make-hash-table :test 'equal))

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (push line *data*))))
(setf *data* (reverse *data*))

(format t "~a~%" (car *data*))

(defun checksum (data)
  ;; process all data
  (loop for id in data do
    ;; process each id
    (loop for c across id do
      (if (gethash c *counts*)
          (incf (gethash c *counts*))
          (setf (gethash c *counts*) 1)))))

(defun show-counts ()
  (loop for k being the hash-keys in *counts*
          using (hash-value v) do
            (format t "~a : ~a~%" k v)))
