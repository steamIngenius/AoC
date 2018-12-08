(defparameter *events* nil)
(defparameter *wonky-time-offset* 500)
(defparameter *guard-profiles* nil)

(defun parse-guard-id (event)
  (parse-integer (second (split (getf event :log) #\#)) :junk-allowed t))

(defun is-shift-change? (event)
  (search "shift" (getf event :log)))

(defun asleep? (event)
  (search "asleep" (getf event :log)))

(defun new-guard (id)
  (list :id id :sleep-patterns (make-array 60 :initial-element 0)))

(defun update-guard (guard asleep awake)
  ;; calc start end of sleep
  ;; update the minutes of the guard's sleep patterns heatmap
  )

(defun process-sleep (guard-id asleep-at awake-at)
  (let ((guard-profile (find guard-id
                             *guard-profiles*
                             :key (extract :id))))
    (if guard-profile
        (setf guard-profile (update-guard guard-profile asleep-at awake-at))
        (setf *guard-profiles*
              (cons (new-guard guard-id) *guard-profiles*)))))

(defun analyze-event-logs (events)
  (defvar current-guard)
  (defvar asleep-at)
  (loop for event in events do
    (cond ((is-shift-change? event) (setf current-guard (parse-guard-id event)))
          ((asleep? event) (setf asleep-at (getf event :timestamp)))
          (t (process-sleep current-guard
                            asleep-at
                            (getf event :timestamp))))))

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
         (hour (parse-integer (first (split time-part #\Colon))))
         (min (parse-integer (second (split time-part #\Colon))))
         (day (parse-integer (third (split date #\-))))
         (month (parse-integer (second (split date #\-))))
         (year (parse-integer (first (split date #\-)))))
    (encode-universal-time 0
                           min
                           hour
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

(analyze-event-logs *events*)

;; DONE
;; read lines and parse into a hash or plist
;; place in an assoc. seq. of events
;; order seq. of events by timestamp
;; process events and create a data structure with:
;;   an entry for each guard
;;
;; TODO
;; process events cont'd...
;;   a heatmap for each guard's sleeping patterns
;; ask 2 questions:
;;   which guard sleeps the most?
;;   which minute is the guard most consistently asleep for?
;; multiply the results
