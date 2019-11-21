(defun variablep (x)
  (let ((str (symbol-name x)))
    (if (eq (elt str 0) #\?)
        (elt str 1))))

(defun aspend (list1 list2)
  (loop for x in list2
        do (setf list1 (acons (car x)
                              (cdr x)
                              list1))))

(defun pattern-match (pat mat &optional vars)
  (cond  ((and pat mat (listp pat) (listp mat))
         (let ((first (pattern-match (first pat) (first mat) vars))
               (rest nil))
           (if first
               (setf rest (pattern-match (rest pat) (rest mat) (list first))))
           (if rest
               )))
        ((variablep pat)
         (if (or (not (assoc pat vars))                ;variable is not defined yet
                 (equal (cdr (assoc pat vars)) mat))   ;value of mat matches the already defined variable
             (cons pat mat)))
        ((eq pat mat)
         (cons t t))
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
