(defparameter *data* ())

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read-line input nil)
          while line do (push line *data*))))

;; read lines and parse into a hash or plist
;; place in an assoc. seq.
;; order seq. of events by timestamp
;; process events and create a data structure with:
;;   an entry for each guard
;;   a heatmap for each guard's sleeping patterns
;; ask 2 questions:
;;   which guard sleeps the most?
;;   which minute is the guard most consistently asleep for?
;; multiply the results
