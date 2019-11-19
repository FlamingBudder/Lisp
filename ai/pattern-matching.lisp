
(defun variablep (x)
  (let ((str (symbol-name x)))
    (if (eq (elt str 0) #\?)
        (elt str 1))))

(defun pat-match (pat mat &optional vars)
  (cond ((equal pat '(?* ?x))
         (cons * *))
        ((and (listp pat) (listp mat))
         (let ((matching t))
           (loop for x in pat
                 for y in mat
                 until (not matching)
                 with a = nil
                 do (setf a (pat-match x y vars))
                 if (and (car a)
                         (or (equal (cdr a) (cdr (assoc (car a) vars)))
                             (not (assoc (car a) vars)))) 
                 do (setf vars (acons (car a) (cdr a) vars))
                 else
                 do (setf matching nil))
           (if matching vars)))
        ((variablep pat)
         (cons pat mat))
        ((eq pat mat)
         (cons T T))
        (t nil)))

(defun seg-match (pat mat &optional vars)
  (let ((matlen (length mat))
        (patlen (length pat))
        (check nil))
    (print (- matlen patlen))
    (loop for x from 0 to (- matlen
                             patlen)
          do (setf check (pat-match pat
                                    (subseq mat x (+ x patlen))
                                    vars))
          if check
          collect (list x check))))
