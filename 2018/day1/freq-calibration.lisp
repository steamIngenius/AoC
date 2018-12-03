(defparameter *data* '())
(defparameter *settings* '(0))

(defun part1-calibrate (data)
  (loop for things in data summing things))

(defun part2-calibrate (data)
  (loop named calibration do
    (loop for item in data do
      (let ((new-setting (+ (car *settings*) item)))
        (if (member new-setting *settings*)
            (return-from calibration new-setting)
            (push new-setting *settings*))))))

(with-open-file (input "input.txt")
  (when input
    (loop for line = (read input nil)
          while line do (push line *data*))))
(setf *data* (reverse *data*))

(format t "The answer for part 1 is: ~a~%" (part1-calibrate *data*))
(format t "The answer for part 2 is: ~a~%" (part2-calibrate *data*))
