(defparameter *data* (make-array (* 1000 1000) :initial-element 0))

(defun split-by-space (str)
  "Returns a list of substrings of str"
  (loop for i = 0 then (1+ j)
        as j = (position #\Space str :start i)
        collect (subseq str i j)
        while j))

(defun parse-rect (str)
  "parse a rectacngle from a string like #218 @ 278,517: 18x19"
  (split-by-space str))

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (parse-rect line))))
