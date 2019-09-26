
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

(defun instant-win-p (list)
  (let ((sum (+ (first list) (second list))))
    (or (= sum 7) (= sum 11))))

(defun instant-loss-p (list)
  (let ((sum (+ (first list) (second list))))
    (or (= sum 2) (= sum 3) (= sum 12))))

(defun say-throw (list)
  (let ((sum (+ (first list) (second list))))
    (cond ((= sum 2)
           'snake-eyes)
          ((= sum 12)
           'boxcars)
          (t sum))))

(defun addon (list x)
  (setf list (append list (list x))))

(defun say-craps ()
  (let ((log '())
        (fir 0))
    (setf fir throw-dice)
    (addon log curr)
    (cond ((instant-win-p curr)
           (addon log 'you-win))
          ((instant-lose-p curr)
           (addon log 'you-lose))
          (t (rest-craps log fir)))))

(defun rest-craps (log fir)
  (let ((game-state 0)
        (win nil))
    ()
