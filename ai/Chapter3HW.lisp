
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
  (let ((x 0))
    (loop repeat 6
          if (= (random 6) 5)
          do (incf x))
    (>= x 1)))

(defun game2 ()
  (let ((win nil))
    (loop repeat 24
          if (= (random 6) (random 6) 5)
          do (setf win t))
    win))

(defun winrate (g n)
  (let ((wins 0))
    (loop repeat n
          if (funcall g)
          do (incf wins))
    (/ wins n)))

                                        ; game1 - 66.099%
                                        ; game2 - 49.102%
                                        ; game 1 easier by 16.997%

(defun symbless (s1 s2)
  (string-lessp (string s1) (string s2)))

(defun set-equal (set1 set2)
  (equal (sort set1 #'symbless) (sort set2 #'symbless)))

(defun left-side (list)
  (loop for x in list until (eq x '-vs-)
        collect x))

(defun right-side (list)
  (rest (member '-vs- list)))

(defun count-common (l1 l2)
  (let ((common 0))
    (loop for x in l1
          do (loop for y in l2
                   if (eq x y)
                   do (incf common)))
    common))

(defun compare (list)
  (list (count-common (left-side list) (right-side list)) 'common 'features))
