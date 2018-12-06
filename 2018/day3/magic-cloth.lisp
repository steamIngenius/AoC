(defparameter *height* 1100)
(defparameter *width* 1100)
(defparameter *fabric* (make-array (* *height* *width*) :initial-element 0))
(defparameter *rects* ())

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
    (loop for y from (second xy) to (+ (second xy) h -1) do
      (let ((iy (* y *width*)))
        (loop for x from (first xy) to (+ (first xy) w -1) do
          (incf (elt *fabric* (+ x iy))))))))

(defun parse-rect (f str)
  "parse a rectacngle from a string like #218 @ 278,517: 18x19"
  (funcall f (mapcar 'parse-integer (split (third (split-by-space (remove #\Colon str))) #\Comma))
        (mapcar 'parse-integer (split (fourth (split-by-space str)) #\x))))

(defun claimp (xy wh)
  ;; test to see if all 1's
  (let ((w (first wh))
        (h (second wh)))
    (every #'identity (loop for y
                            from (second xy)
                              to (+ (second xy) h -1)
                            collect (let ((iy (* y *width*))
                                          (x (first xy)))
                                      (every #'(lambda (x) (= x 1)) (subseq *fabric* (+ x iy) (+ x iy w))))))))

(defun find-claim (rects)
  (loop for line in rects do
       (if (parse-rect 'claimp line)
           (format t "answer: ~a~%" line)
           )
       )
  )

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (progn
                          (parse-rect 'plot line)
                          (push line *rects*)))))

(let ((total (count 1 *fabric* :test '<)))
  (format t "...done. ~a inches of overlap~%" total))

(find-claim *rects*)
