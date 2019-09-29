(defparameter *rules*
  '((0 7 1)
    (0 11 1)
    (0 2 -1)
    (0 3 -1)
    (0 12 -1)
    (0 point 0)
    (point point 1)
    (point 7 -1)
    (point x 0)))

(defun replacevar (list var val)
  (loop for x in list
        if (equal var x)
        collect val
        else
        collect x))

(defun follows (list rule)
  (and (if (symbolp (first rule))
           (setf rule (replacevar rule (first rule) (first list)))
           t)
       (if (symbolp (second rule))
           (setf rule (replacevar rule (second rule) (second list)))
           t)
       (and (= (first list) (first rule))
            (= (second list) (second rule)))))

(defun simulate-1 (list)
  (loop for x in *rules*
        if (follows list x)
        return (third x)))

(defun simulate ()
  (let ((log '())
        (state 0)
        (roll 0)
        (point 0))
    (loop while (= state 0)
          do (setf roll (+ (+ (random 6) 1) (+ (random 6) 1)))
          do (setf state (simulate-1 (list point roll)))
          do (setf log (append log (list roll)))
          if (= point 0)
          do (setf point roll))
    (if (= state 1)
        (append log (list 'you-win))
        (append log (list 'you-lose)))))
