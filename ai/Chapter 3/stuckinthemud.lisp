(defun roll (n)
  (loop repeat n
        collect (+ (random 6) 1)))

(defun remove-stuck (list)
  (remove 5 (remove 2 list)))

(defun score (list)
  (apply #'+ list))

(defun stuck-list ()
  (butlast (let ((left 5)
                 (roll '()))
             (loop until (= left 0)
                   do (setf roll (roll left))
                   do (setf left (length (remove-stuck roll)))
                   collect roll))))

(defun counted (list)
  (loop for x in list
        if (not (or (member 2 x)
                    (member 5 x)))
        collect x))

(defun stuck-score (list)
  (apply #'+ (apply #'append (counted list))))
