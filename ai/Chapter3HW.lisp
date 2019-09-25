
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

(defun game1 ()
  (let ((x 0)
        (n 6))
    (loop while (> n 0)
          do (decf n)
          if (= (random 6) 5)
          do (incf x))
    (>= x 1)))

(defun game2 ()
  (let ((win nil)
        (n 24))
    (loop while (> n 0)
          do (decf n)
          if (= (random 6) (random 6) 5)
          do (setf win t)
          do (setf n 0))
    win))

(defun winrate (g n)
  (let ((wins 0)
        (orig n))
    (loop while (> n 0)

          do (decf n)
          if (funcall g)
          do (incf wins))
    (/ wins orig)))

                                        ; game1 - 66.099%
                                        ; game2 -  2.873%
                                        ; game 1 easier by 63.226% or ~23 times easier

(defun left-side (list)
  (loop while (not (equal (first list) '-vs-))
        collect (first list)
        do (setf list (rest list))))

(defun right-side (list)
  (let ((current nil))
    (loop while (not (equal current '-vs-))
          do (setf current (first list))
          do (setf list (rest list)))
    (loop while (> (length list) 0)
          collect (first list)
          do (setf list (rest list))))
    )

(defun count-common (l1 l2)
  (let ((common 0))
    (loop for x in l1
          do (loop for y in l2
                   if (eq x y)
                   do (incf common)))
    common))

(defun compare (list)
  (count-common (left-side list) (right-side list)))
