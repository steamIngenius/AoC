(defparameter *events* nil)
(defparameter *wonky-time-offset* 500)

(defun split (str chr)
  "Returns a list of substrings of str split by chr"
  (loop for i = 0 then (1+ j)
        as j = (position chr str :start i)
        collect (subseq str i j)
        while j))

(defun extract-timestamp (str)
  (let* ((raw (subseq str 1 (position #\] str :start 1)))
         (date-time (split raw #\Space))
         (date (first date-time))
         (time-part (second date-time))
         (min (parse-integer (second (split time-part #\Colon))))
         (day (parse-integer (third (split date #\-))))
         (month (parse-integer (second (split date #\-))))
         (year (parse-integer (first (split date #\-)))))
    (encode-universal-time 0
                           min
                           0
                           day
                           month
                           ;; 1518 is too early for universal time lol
                           (+ year *wonky-time-offset*))))

(defun extract-log (str)
  (subseq str (+ 2 (position #\] str))))

(defun parse-event (event)
  (let ((timestamp (extract-timestamp event))
        (log-entry (extract-log event))
        )
    (list :timestamp timestamp :log log-entry :event event)))

(defun injest (line)
  "parse lines into events and place into a seq"
  (setf *events* (cons (parse-event line) *events*)))

(defun extract (property)
  (lambda (plist)
    (getf plist property)))

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (injest line))))

(setf *events* (sort *events* #'< :key (extract :timestamp)))


;; DONE
;; read lines and parse into a hash or plist
;; place in an assoc. seq. of events
;; order seq. of events by timestamp
;;
;; TODO
;; process events and create a data structure with:
;;   an entry for each guard
;;   a heatmap for each guard's sleeping patterns
;; ask 2 questions:
;;   which guard sleeps the most?
;;   which minute is the guard most consistently asleep for?
;; multiply the results
