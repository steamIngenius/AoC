(defparameter *height* 0)
(defparameter *width* 0)
(defparameter *destinations* ())
(defparameter *exclusions* (make-hash-table))
(defparameter *areas* (make-hash-table))

(defun split (str chr)
  "Returns a list of substrings of str split by chr"
  (loop for i = 0 then (1+ j)
        as j = (position chr str :start i)
        collect (subseq str i j)
        while j))

(defun split-by-space (str)
  (split str #\Space))

(defun parse-location (raw)
  (mapcar
   'parse-integer
   (split-by-space (remove #\Comma raw))))

(format t "Reading coordinates.~%")
(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          for id from 0
          while line do (push (cons id (parse-location line)) *destinations*))))

(loop for (nil x y) in *destinations*
      do
         (setf *height* (max x *height*))
         (setf *width* (max y *width*)))
(defparameter *map* (make-array (* *height* *width*) :initial-element (list :id nil :proximity 0)))

(defun add-to-map (x y id proximity)
  (setf (elt *map* (+ x (* y *width*)))
        (list :id id :proximity proximity)))

(defun get-xy (x y)
  (elt *map* (+ x (* y *width*))))

(defun proximity (p1 p2)
  "Returns the manhattan distance between two points."
  (destructuring-bind ((x1 y1) (x2 y2)) (list p1 p2)
    (+ (abs (- x2 x1)) (abs (- y2 y1)))))

(format t "Mapping proximities...")
(do ((x 0 (+ x 1)))
    ((>= x *width*))
  (do ((y 0 (+ y 1)))
      ((>= y *height*))
    (loop for (id dx dy) in *destinations* do
      (let ((p (proximity (list dx dy) (list x y)))
            (current (get-xy x y)))
        (cond ((or (< p (getf current :proximity))
                   (= p 0)
                   (not (getf current :id)))
               (add-to-map x y id p))
              ((= p (getf current :proximity)) (add-to-map x y "." p)))))))
(format t "done.~%")

;; calculate areas, identify 'infinite' areas and add to exclusions
(do ((x 0 (+ x 1)))
    ((>= x *width*))
  (do ((y 0 (+ y 1)))
      ((>= y *height*))
    (if (or (= x 0)
            (= x (- *width* 1))
            (= y 0)
            (= y (- *height* 1)))
        (setf (gethash (getf (get-xy x y) :id) *exclusions*) t)
        (multiple-value-bind (value present) (gethash (getf (get-xy x y) :id) *areas*)
          (declare (ignore value))
          (if present
              (incf (gethash (getf (get-xy x y) :id) *areas*))
              (setf (gethash (getf (get-xy x y) :id) *areas*) 1))))))

;; find the largest area, ignore exclusions
(defparameter *largest* 0)
(maphash #'(lambda (k v) (if (gethash k *exclusions*) () (setf *largest* (max *largest* v)))) *areas*)

