(ql:quickload :lisp-unit)
(use-package :lisp-unit)
(setf *print-failures* t)


(defun one-more-mult (n)
  "* Make sure you know how to use (mod x n) to figure out if x is
divisible by n. Write a function that takes in an integer and returns
true when the integer is one more than a multiple of 8."
  (= (mod n 8) 1))

(defun all-6-by-17 (xs)
  "* Given a list of integers, return all of the elements that are
either greater than 100 or leave a remainder of 6 when divided by 17."
  (loop for x in xs
        if (or (> x 100)
               (= (mod x 17) 6))
        collect x))

(defun long-words (wordlist)
  "* Given a list of words, return the number of words in the list that
  are longer than 6 letters."
  (let ((n 0))
    (loop for x in wordlist
          if (> (length x) 6)
          do (incf n))
    n))

(defun good-lists (ys)
  "Given a list of lists `ys`, if a sublist y of ys begins with the
symbol `GOOD`, then put every element from the list y in the answer."
  (apply #'append (loop for y in ys
                        if (equal (first y) 'good)
                        collect y)))

(defun x010 (pts)
  (loop for x in pts
        if (and (<= (first x) 10)
                (>= (first x) 0))
        collect (first x)))

(defun y200 (pts)
  "* (`y200`) Return a list of all of the points whose y values are 
  either greater than 200 or less than -200."
  (loop for pt in pts
        if (or (< (second pt) -200)
               (> (second pt) 200))
        collect pt))

(defun ptffun (x y)
  (+ (expt x 2)
     (* 3 (expt y 2))
     (* -2 x y)))

(defun ptf (pts)
  "* (`ptf`) Find the greatest value of 
f(x,y)=x2+3y2−2xy
 using the
  points in the list."
  (loop for pt in pts
        (let ((x (first pt))
              (y (second pt)))
          )))

(defun aop (pts)
  "* (`aop`) If every point is on the parabola y=x2
 then return true (otherwise false)."
  nil)




(define-test test-one-more-mult
  "Test one-more-mult"
  (assert-false (one-more-mult 8))
  (assert-false (one-more-mult 4))
  (assert-false (one-more-mult 5))
  (assert-false (one-more-mult 15))
  (assert-true (one-more-mult 9))
  (assert-true (one-more-mult 17)))



(define-test test-all-6-by-17
  "Test all-6-by-17"
  (assert-equal `(103 105 23 40 -28)
		(all-6-by-17 `(50 60 70 -30 103 80 105 22 23 24 34 40 51 -28))))




(define-test test-longwords
  "Vocab check"
  (assert-equal 3
		(long-words (list "short" "transcendental" "transmogrify"
				  "longer" "stuff" "cuddles" "finish"))))



(define-test test-good-lists
  "Example to help you decode the question."
  (assert-equal `(GOOD LUCK GOOD RIDDANCE)
		(good-lists `((BAD START) (GOOD LUCK) (EVIL EMPIRE)
			                  (GOOD RIDDANCE) (YEET)))))



(define-test test-x010
    (assert-equal '(0 3 1 9)
		  (x010 `((0 5) (3 8) (-7 4) (1 13) (15 3) (9 20)))))



  (define-test test-y200
      (assert-equal '((40 -300) (50 201) (5 -200) (50 500))
		    (y200 '((40 -300) (50 201) (-205 90)
			    (5 -200) (50 500) (400 95)))))

  

  (define-test test-ptf
    "What is the greatest value of ptf?"
    (assert-equal 225000 (ptf '((40 20) (50 200) (-90 40)))))

  (defun smd (pts)
    "* (`smd`) Find the smallest difference $\abs{x-y}$ in the list."
    0)

  (define-test test-smd
    "What is the smallest difference? |x-y|"
    (assert-equal 10 (ptf '((40 20) (50 200) (-90 -80) (800 30)))))


  

  (define-test test-aop
    "Are points on parabola?"
    (assert-true (aop '((5 25) (6 36) (9 81))))
    (assert-false (aop '((5 25) (6 36) (7 50) (9 81)))))

  (defun isfar (pts)
    "* (`isfar`) If any point has $\abs{y - x^2} > 10$, then return true."
    nil)

  (define-test test-isfar
    "isFar(), technically"
    (assert-true (isfar '((5 25) (6 47) (9 81))))
    (assert-true (isfar '((5 25) (6 25) (9 81))))
    (assert-false (isfar '((5 24) (6 37) (9 79)))))

  (defun not10x (pts)
    "* (`not10x`) If $y \not= 10^x$ for at least one (x,y) pair in the list, return true."
    nil)

  (define-test test-not10x
    "Not 10x"
    (assert-false (not10x '((2 100) (3 1000))))
    (assert-true (not10x '((2 10000) (4 100000)))))

  (defun xyzTrip (pts)
    "* (`xyzTrip`) Given a list of triples 
(
x
,
y
,
z
)
, which we write `(list x y z)`, 
  return a list containing `(list x y)` for every point where 
z
=
x
2
+
y
2
."
    (list (list 0 1)))

  (define-test test-xyzTrip
    "Triples?!"
    (assert-equal '((5 12) (20 21))
		  (xyzTrip '((5 12 169) (8 15 17) (20 21 841)))))
  
  (run-tests)
