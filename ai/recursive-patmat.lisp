(defun variablep (x)
  (let ((str (symbol-name x)))
    (if (eq (elt str 0) #\?)
        (elt str 1))))

(defun combine (a b)
  (loop for x in a
        do (setf b (acons (car x) (cdr x) b)))
  b)

(defun checkstar (x pat)
  (and (listp pat)
       (eq x (first pat))))

(defun pattern-match (pat mat &optional vars)
  (cond ((and pat (listp pat) (equal (first pat)
                                  '?*))
         (acons (first pat) (second pat) vars))
        ((and pat mat (listp pat) (listp mat))
         (setf vars (pattern-match (first pat) (first mat) vars))
         (if vars
             (if (assoc '?* vars)
                 (let ((nextpos (or (position '?* (rest pat) :test #'checkstar) (length (rest pat)))))
                   (setf vars (loop for x in (seg-match (rest pat)
                                                        mat
                                                        vars)
                                    thereis (pattern-match (subseq (rest pat) nextpos)
                                                           (subseq mat (first x))
                                                           (remove '?* (second x) :test #'(lambda (x mat) (eq x (first mat))))))))
                 (setf vars (pattern-match (rest pat) (rest mat) vars))))
         vars)
        ((and (symbolp pat) (variablep pat))
          (if (or (not (assoc pat vars))                ;variable is not defined yet
                  (equal (cdr (assoc pat vars)) mat))   ;value of mat matches the already defined variable
              (acons pat mat vars)))
         ((equal pat mat)
          (acons t t vars))
         (t nil)))

(defun seg-match (pat mat &optional vars)
  (setf pat (subseq pat 0 (position '?* pat :test #'checkstar))) ;get all of pat before the next ?*
  (let ((matlen (length mat))
        (patlen (length pat))
        (check nil))
    (loop for x from 0 to (- matlen   ;all possible positions in which the ?* stuff ends
                             patlen)
          do (setf check (pattern-match pat  ;check if stuff before the next ?* in pattern matches with corresponding pos in match
                                        (subseq mat x (+ x patlen))
                                        (remove '?* vars :test #'(lambda (x mat) (eq x (first mat))))))
          if (and check
                  (or (not (assoc (cdr (assoc '?* vars)) vars))
                      (equal (cdr (assoc (cdr (assoc '?* vars)) vars))
                             (subseq mat 0 x))))
          collect (list (+ x patlen)
                        (acons (cdr (assoc '?* vars))
                               (subseq mat 0 x)
                               check)))))
