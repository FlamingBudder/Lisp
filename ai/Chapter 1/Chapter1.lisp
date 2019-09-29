(defun f (x) x)
(defun g (y) y)

(defun mappend (fn the-list)
  "Apply fn to each element of lis t and append the results."
  (apply #'append (mapcar fn the-list))) 

(defvar list1 '(5 4 3 2 1))
(defvar list2 '(0 0 0 0 0))

(setf list2 (list 0 1 2 3 4 5))

(defun self-and-double (x) (list x (+ x x)))

(defun negation (list)
  (mappend #'(lambda (x)
               (if (numberp x)
                   (list x (- x)))) list))


(defvar myfunct #'(lambda (x) (+ x 1)))




;;Exercises

;;1.1
(defparameter *endings* '(Jr. MD PHD Sr.))
(defun last-name (name)
  (if (member (first (last name)) *endings*)
      (last-name (reverse (rest (reverse name))))
      (first (last name))))

;;1.2
(defun power (a b)
  (cond ((= b 0) 1)
        ((> b 0) (* a (power a (- b 1))))
        (t (/ (power a (+ b 1)) a))))

;;1.3
(defun count-atoms (list)
  (cond ((= (length list) 0) 0)
        ((listp (first list)) (+ (count-atoms (first list)) (count-atoms (rest list))))
        (t (+ 1 (count-atoms (rest list))))))

;;1.4
(defun count-anywhere (x list)
  (cond ((= (length list) 0) 0)
        ((listp (first list)) (+ (count-anywhere x (first list)) (count-anywhere x (rest list))))
        ((eq (first list) x) (+ 1 (count-anywhere x (rest list))))
        (t (count-anywhere x (rest list)))))
