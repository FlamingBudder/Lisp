(defun variablep (x)
  (let ((str (symbol-name x)))
    (if (eq (elt str 0) #\?)
        (elt str 1))))

(defun combine (a b)
  (loop for x in a
        do (setf b (acons (car x) (cdr x) b)))
  b)

(defun pattern-match (pat mat &optional vars)
  (cond  ((and pat mat (listp pat) (listp mat))
          (setf vars (pattern-match (first pat) (first mat) vars))
          (if vars
              (setf vars (pattern-match (rest pat) (rest mat) vars)))
          vars)
        ((variablep pat)
         (if (or (not (assoc pat vars))                ;variable is not defined yet
                 (equal (cdr (assoc pat vars)) mat))   ;value of mat matches the already defined variable
             (acons pat mat vars)))
        ((eq pat mat)
         (acons t t vars))
        (t nil)))

(defun seg-match (pat mat &optional vars)
  (let ((matlen (length mat))
        (patlen (length pat))
        (check nil))
    (loop for x from 0 to (- matlen
                             patlen)
          do (setf check (pattern-match pat
                                    (subseq mat x (+ x patlen))
                                    vars))
          if check
          collect (list x check))))
