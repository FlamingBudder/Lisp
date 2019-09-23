(defun constrain (x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (t x)))

(defun mean (list)
  (/ (apply #'+ list) (length list)))

(defun stddev (list)
  (let ((mean (mean list)))
    (sqrt (mean (loop for x in list
                      collect (expt (- x mean) 2))))))

(defun howcompute (a b c)
  (cond ((= (+ a b) c)
         'add)
        ((= (- a b) c)
         'subtract)
        ((= (- b a) c)
         'subtract)
        ((= (* a b) c)
         'multiply)
        (t 'beats-me)))

(defun play (p1 p2)
  (cond ((eq p1 p2)
         '0)
        (or (and (eq p1 'rock) (eq p2 'scissors))
            (and (eq p1 'scissors))
