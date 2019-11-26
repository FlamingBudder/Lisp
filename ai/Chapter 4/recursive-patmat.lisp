(defun variablep (x)
  (let ((str (symbol-name x)))
    (if (eq (elt str 0) #\?)
        (elt str 1))))

(defun combine (a b)
  (loop for x in a
        do (setf b (acons (car x) (cdr x) b)))
  b)

(defun pattern-match (pat mat &optional vars)
  (cond ((and pat mat (listp pat) (equal (first pat)
                                         '(?* ?x)))
         (print "star")
         (acons '?*
                (second (first pat))
                vars))
    ((and pat mat (listp pat) (listp mat))
          (setf vars (pattern-match (first pat) (first mat) vars))
          (if vars
              (if (assoc '?* vars)
                  (loop for x in (seg-match (subseq pat 0 (position '?* pat :test #'checkstar))
                                            mat
                                            vars)
                        with a = nil
                        do (setf a (subseq mat
                                           0
                                           (- (first x)
                                              (length (subseq pat 0 (position '?* pat :test #'checkstar))))))
                        thereis (pattern-match (rest pat)
                                               (subseq mat
                                                       (first x))
                                               (remove-star (rest x))))
                  (setf vars (pattern-match (rest pat) (rest mat) vars))))
          vars)
         ((variablep pat)
          (if (or (not (assoc pat vars))                ;variable is not defined yet
                  (equal (cdr (assoc pat vars)) mat))   ;value of mat matches the already defined variable
              (acons pat mat vars)))
         ((eq pat mat)
          (acons t t vars))
         (t nil)))

(defun remove-star (vars)
  (loop for x in vars
        if (not (eq (car x)
                    '?*))
        collect x))

(defun seg-match (pat mat &optional vars)
  (let ((matlen (length mat))
        (patlen (length pat))
        (check nil))
    (loop for x from patlen to matlen
          do (setf check (pattern-match pat
                                        (subseq mat (- x patlen) x)
                                        vars))
          if check
          collect (list x check))))

(defun checkstar (x pat)
  (and (listp pat)
       (eq x (first pat))))
