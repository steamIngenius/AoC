(defparameter *polymer* ())
(defparameter *alphabet* (loop for c from (char-code #\a)
                                 to (char-code #\z)
                               collect (code-char c)))

(defun reaction (a b)
  (and
   (char-equal a b)
   (not (char= a b))))

(defun react (chain)
  (if (< (length chain) 20) (format t "~a~%" chain))
  (do ((index 0 (1+ index))
       (reacting nil)
       (reaction-occurred nil)
       (newchain (make-array 0 :fill-pointer 0 :adjustable t)))
      ((>= index (- (length chain) 1))
       (if (not reacting)
           (vector-push-extend (elt chain index) newchain))
       (if reaction-occurred
           (react newchain)
           chain))
    (cond ((not reacting)
           (setf reacting (reaction (elt chain index) (elt chain (1+ index))))
           (if reacting (setf reaction-occurred t)
               (vector-push-extend (elt chain index) newchain)))
          (reacting
           (setf reacting nil)))))

(with-open-file (input "input.txt")
  (when input
    (setf (getf *polymer* :raw) (read-line input nil))

    (format t "The answer for part 1 is: ~a~%"
            (length (react (getf *polymer* :raw))))

    (format t "Improving polymers...")
    (setf (getf *polymer* :processed)
          (loop for c in *alphabet*
                collect (list
                         :char c
                         :polymer (react (remove c (getf *polymer* :raw) :test #'char-equal)))))
    (setf (getf *polymer* :processed)
          (sort (getf *polymer* :processed)
                #'<
                :key (lambda (plist)
                       (length (getf plist :polymer)))))
    (format t "done.~%")
    (format t "The answer for part two is: ~a~%"
            (length (getf (first (getf *polymer* :processed)) :polymer)))))

