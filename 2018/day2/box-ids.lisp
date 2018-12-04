(defparameter *data* '())
(defparameter *counts* (cons 0 0))

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (push line *data*))))
(setf *data* (reverse *data*))

(format t "~a~%" (car *data*))

(defun has (x data)
  (loop for v being the hash-values of data do
    (if (= x v) (return t))))

(defun checksum (data)
  ;; process all data
  (loop for id in data do
    ;; process each id
    (let ((counts (make-hash-table :test 'equal)))
      (loop for c across id do
        (if (gethash c counts)
            (incf (gethash c counts))
            (setf (gethash c counts) 1)))
      (if (has 2 counts) (incf (first *counts*)))
      (if (has 3 counts) (incf (rest *counts*)))))
  ;; display the results of our checksum
  (format t
          "The answer for part 1 is: ~a~%"
          (* (first *counts*) (rest *counts*))))

(defun diff (a b)
  (loop for x across a
        for y across b
        if (char/= x y) count it))

(defun common (a b)
  (loop for x across a
        for y across b
        if (char= x y) collect x))

(defun find-ids (data)
  (loop named outer for x in data do
    (loop for y in data do
      (if (= 1 (diff x y))
          (return-from outer (coerce (common x y) 'string))))))

;; (defun show-counts ()
;;   (loop for k being the hash-keys in *counts*
;;           using (hash-value v) do
;;             (format t "~a : ~a~%" k v)))
