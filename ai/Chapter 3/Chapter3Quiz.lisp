
(defun m555 (n)
  (loop while (not (= (/ n 5) (floor (/ n 5))))
        do (incf n))
  (loop for x from n to (+ n 270) by 5
        collect x))

(defparameter *atlas*
  '((pennsylvania pittsburg)
    (pennsylvania harrisburg)      
    (new-jersey newark)
    (ohio columbus)))

(defun cities (state)
  (loop for x in *atlas*
        if (eq (first x) state)
        collect (second x)))

(defun throw-dice ()
  (list (+ 1 (random 6)) (+ 1 (random 6))))

(defun say-throw (list)
  (let ((sum (+ (first list) (second list))))
    (cond ((equal sum 2)
           'snake-eyes)
          ((equal sum 12)
           'boxcars)
          (t sum))))

(defun instant-win-p (sum)
  (or (equal sum 7) (equal sum 11)))

(defun instant-loss-p (sum)
  (or (equal 'snake-eyes sum)
      (equal 'boxcars sum)
      (equal 3 sum)))

(defun writelog (log x)
  (append log (list x)))

(defun say-craps ()
  (let ((log '())
        (roll (say-throw (throw-dice))))
    (cond ((instant-win-p roll)
           (writelog (writelog log roll) 'you-win))
          ((instant-loss-p roll)
           (writelog (writelog log roll) 'you-lose))
          (t (rest-craps (writelog (writelog (writelog (writelog log 'your) 'point) 'is) roll) roll)))))

(defun rest-craps (log first)
  (let ((roll (say-throw (throw-dice))))
    (cond ((equal roll first)
           (writelog (writelog log roll) 'you-win))
          ((equal roll 7)
           (writelog (writelog log roll) 'you-lose))
          (t (rest-craps (writelog log roll) first)))))

(defun winrate (g n)
  (let ((wins 0))
    (loop repeat n
          if (eq (first (last (funcall g))) 'you-win)
          do (incf wins))
    (float (* 100 (/ wins n)))))
