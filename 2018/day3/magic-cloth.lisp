(defparameter *data* (make-array (* 1000 1000) :initial-element 0))

(defun split (str chr)
  "Returns a list of substrings of str split by chr"
  (loop for i = 0 then (1+ j)
        as j = (position chr str :start i)
        collect (subseq str i j)
        while j))

(defun split-by-space (str)
  "Returns a list of substrings of str split by the space character"
  (split str #\Space))

(defun plot (xy wh)
  (let ((w (first wh))
        (h (second wh)))
    (loop for y from (second xy) to (+ (second xy) h) do
      (loop for x from (first xy) to (+ (first xy) w) do
        (let ((i (+ x (* y w))))
          (incf (elt *data* i)))))))

(defun parse-rect (str)
  "parse a rectacngle from a string like #218 @ 278,517: 18x19"
  (plot (mapcar 'parse-integer (split (third (split-by-space (remove #\Colon str))) #\Comma))
        (mapcar 'parse-integer (split (fourth (split-by-space str)) #\x))))

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (parse-rect line))))

(let ((total (count 1 *data* :test '<)))
  (format t "...done. ~a~%" total))
