(defparameter *rules*
  (pairlis '(?gerbil ?dog ?cat ?+ ?* ?random)
           '((lambda (name activity)
               `(The gerbil ,name loves to ,activity))
             (lambda (name &optional (barks 0))
               (append `(,name loves to bark.)
                       (loop repeat barks
                             collect 'Bark!)))
             (lambda (name)
               (append `(The cat ,name loves to)
                       (list (elt '(purr meow raawr)
                                  (random 3)))))
             #'+
             #'*
             (lambda (&rest vals)
               (elt vals (random (length vals))))
             )))

(defun matcher (list)
  (apply (eval (cdr (assoc (first list)
                           *rules*)))
         (rest list)))
