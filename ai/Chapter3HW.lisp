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
        ((or (and (eq p1 'rock) (eq p2 'scissors))
             (and (eq p1 'scissors) (eq p2 'paper))
             (and (eq p1 'paper) (eq p2 'rock)))
         '1)
        (t '2)))

(defun palindromep (list)
  (if (equal (rev list) list)
      t
      nil))

(defun rev (l)
   (cond
     ((null l) '())
     (T (append (rev (cdr l)) (list (car l)))))) 

(defun make-palindrome (list)
  (append list (rev list)))

(defun squarechance (s e)
  (let ((c 0))
    (loop for x from s
          while (< (expt x 2) e)
          do (incf c))
    (/ c (+ 1 (- e s)))))

(defun roll (n)
  (let ((x 0))
    (loop while (> n 0)
          do (decf n)
          if (= (random 6) 5)
          do (incf x))
    x))

(defun test-game ())
