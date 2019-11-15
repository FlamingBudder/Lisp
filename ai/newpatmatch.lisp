(defun variablep (x)
  (eq (elt (symbol-name x) 0)
      #\?))

(defun patmatch (pat mat vars)
  (cond ((and (listp pat) (listp mat)
              (= (length pat) (length mat)))
         (loop for x in pat
               for y in mat
               with a = nil
               do (setf a (patmatch x y))
               
               ))
        ((variablep pat)
         (cons pat mat))
        ((eq pat mat)
         (cons T T))
        (t nil)))
