(defparameter *polymer* ())
(defparameter *alphabet* (loop for c from (char-code #\a)
                                 to (char-code #\z)
                               collect (code-char c)))

(defun reaction (a b)
  (and
   (char-equal a b)
   (not (char= a b))))

(defun react (chain)
    (let ((reaction-occurred nil)
          (skip nil))
      (loop for unit across chain
            for i from 0
            with end = (1- (length chain))
            if (and (not skip)
                    (< i end)
                    (not (reaction unit (elt chain (1+ i)))))
              collect unit
            else do
              (cond (skip (setf skip nil))
                    ((< i (1- (length chain))) nil)
                    (t (when (reaction unit (elt chain (1+ i)))
                         (setf reaction-occurred t)
                         (setf skip t)))))))

(with-open-file (input "input.txt")
  (when input
    (setf (getf *polymer* :raw) (read-line input nil))
    (format t "~a~%" (length (getf *polymer* :raw)))
    (format t "~a~%" (length (react (getf *polymer* :raw))))
    ;; (loop for c in *alphabet*
    ;;       with polymer = ""
    ;;       collect (list :char c :polymer polymer))
    ))

;; grab a char
;; if there was a reaction
;;   reset reaction flag and discard char
;; otherwise
;;   check to see if a reaction happens with this char and the next
;; reaction yes
;;   set flag and discard this char
;; reaction no
;;   collect



;; (defun react (chain)
;;   (loop named react do
;;     (let ((reaction-occurred nil))
;;       (loop for unit across chain
;;             for i from 0 do
;;               (when (and (< i (1- (length chain)))
;;                          (reaction unit (elt chain (1+ i))))
;;                 (setf reaction-occurred t)
;;                 (setf (char chain i) #\*)
;;                 (setf (char chain (1+ i)) #\*)))
;;       (if reaction-occurred
;;           (progn
;;             (format t ".")
;;             (setf (getf *polymer* :raw) (remove #\* chain))
;;             (setf reaction-occurred nil))
;;           (return-from react))))
;;   )
